{-# LANGUAGE CPP #-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module: GHC.TcPlugin.API.TyConSubst

This module provides functionality for recognising whether a type is a
'TyConApp' while taking into account Given constraints.

In particular, this allows dealing with flattening variables on older GHC versions
(9.0 and below). For example, instead of @[W] m + n ~ n + m@, older GHCs might
produce @[G] u ~ m + n, [G] v ~ n + m, [W] u ~ v@. In this case, one needs to
use the Givens to recognise that @u ~ v@ can be solved using the laws of natural
number arithmetic.

Usage:

  - Use 'mkTyConSubst' to create a 'TyConSubst' from Givens.
  - Use 'splitTyConApp_upTo' to compute whether a type is a 'TyConApp', taking
    into account the Givens (in the form of the 'TyConSubst').

Note that 'splitTyConApp_upTo' will also look through type synonyms.

-}
module GHC.TcPlugin.API.TyConSubst (
    TyConSubst -- opaque
  , mkTyConSubst
  , splitTyConApp_upTo
  ) where

-- base
import Data.Bifunctor
import Data.Either
  ( partitionEithers )
import Data.Foldable
  ( toList, asum )
import Data.List.NonEmpty
  ( NonEmpty(..) )

-- containers
import Data.Graph
  ( Graph, Vertex )
import qualified Data.Graph as Graph
import Data.Map
  ( Map )
import qualified Data.Map as Map
import Data.Set
  ( Set )
import qualified Data.Set as Set

-- ghc
import GHC.Utils.Outputable
  hiding ( (<>) )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
import GHC.Tc.Types.Constraint

{-------------------------------------------------------------------------------
  The main type

  TODO: maybe this could be sped up with
  <https://hackage.haskell.org/package/union-find>?
-------------------------------------------------------------------------------}

-- | Substitution for recognizing 'TyCon' applications modulo equalities
--
-- During constraint solving the set of " given " constraints includes so-called
-- "canonical equalities": equalities of the form
--
-- > var ~ typ                  (CTyEqCan)
-- > var ~ TyCon arg1 .. argN   (CFunEqCan, the TyCon will be a type family)
--
-- The problem we want to solve is recognizing if some type τ is of the form
--
-- > TyCon arg1 arg2 .. argN   (0 <= N)
--
-- modulo those canonical equalities. We limit the scope of what we try to do:
--
-- o We are only interested in recognizing types of the form above
--   (as opposed to general parsing-modulo-equalities).
-- o We will only use the canonical equalities as-is: we will not attempt to
--   derive any additional equalities from them (i.e. if, say, we know that
--   @x ~ T1@ and @x ~ T2@, we will not attempt to use the fact that this means
--   that @T1 ~ T2@, nor any derived conclusions thereof). We /will/ however
--   try to apply the canononical equalities as often as is necessary (e.g.,
--   first applying @x ~ T y@, then applying @y ~ T2@).
--
-- We solve this problem by constructing a 'TyConSubst': a possibly
-- non-deterministic substitution mapping type variables to types of the form
-- above (that is, a type constructor applied to some arguments).
--
-- We detail the construction of this substitution below (see documentation of
-- 'Classified' and 'process'), but once we have this substitution, the
-- recognition problem becomes easy:
--
-- 1. Without loss of generality, let τ be of the form @t arg1 arg2 .. argN@
-- 2. If @t@ is a 'TyCon', we're done.
-- 3. Otherwise, if @t@ is a variable @x@, lookup @x@ in the substitution; if
--    there is one (or more) mappings for @x@, then we have successfully
--    recognized τ to be of the form above. There is no need to apply the
--    substitution repeatedly.
--
-- The substitution is non-deterministic because there might be multiple
-- matches. For example, if we have
--
-- > type family Foo where
-- >   Foo = Int
--
-- then we might well have equalities @x ~ Int, x ~ Foo@ in scope, and so a type
-- @x@ would match two different 'TyCon's. What we do know, however, is that if
-- τ matches both @t arg1 .. argN@ and @t' arg1' .. argM'@ (possibly @N /= M@),
-- then
--
-- > t arg1 .. argN ~ t' arg1' .. argM'
--
-- If @t == t'@, we can conclude that the arguments are equal only if @t@ is
-- injective.
data TyConSubst = TyConSubst {
      -- | Mapping from (canonical) variables to 'TyCon' applications
      tyConSubstMap :: Map TcTyVar (NonEmpty (TyCon, [Type]))

      -- | Map each variable to the canonical representative
      --
      -- See 'Classified' for a detailed discussion of canonical variables.
    , tyConSubstCanon :: Map TcTyVar TcTyVar
    }

{-------------------------------------------------------------------------------
  Basic functionality for working with 'TyConSubst'
-------------------------------------------------------------------------------}

-- | Empty substitution
--
-- The canonical variables map is established once when the initial substitution
-- is generated and not updated thereafter.
tyConSubstEmpty :: Map TcTyVar TcTyVar -> TyConSubst
tyConSubstEmpty canon = TyConSubst {
      tyConSubstMap   = Map.empty
    , tyConSubstCanon = canon
    }

-- | Lookup a variable in the substitution
tyConSubstLookup :: TcTyVar -> TyConSubst -> Maybe (NonEmpty (TyCon, [Type]))
tyConSubstLookup var TyConSubst{..} = Map.lookup var' tyConSubstMap
  where
    var' :: TcTyVar
    var' = canonicalize tyConSubstCanon var

-- | Extend substitution with new bindings
tyConSubstExtend ::
     [(TcTyVar, (TyCon, [Type]))]
  -> TyConSubst -> TyConSubst
tyConSubstExtend new subst@TyConSubst{..} = subst {
      tyConSubstMap = Map.unionWith (<>)
                        (Map.fromList $ map (uncurry aux) new)
                        tyConSubstMap
    }
  where
    aux :: TcTyVar -> (TyCon, [Type]) -> (TcTyVar, NonEmpty (TyCon, [Type]))
    aux var s = (canonicalize tyConSubstCanon var, s :| [])

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

-- | Classified canonical equality constraints
--
-- The first step in the construction of the 'TyConSubst' is to classify the
-- available canonical equalities as one of three categories, defined below.
data Classified = Classified {
      -- | " Obviously " productive mappings
      --
      -- An equality @var := TyCon args@ is productive, because as soon as we
      -- apply it, we are done: we have successfully recognized a type as being
      -- an application of a concrete type constructor (note that we only ever
      -- apply the substitution to the head @t@ of a type @t args@, never to the
      -- arguments).
      classifiedProductive :: [(TcTyVar, (TyCon, [Type]))]

      -- | Extend equivalence class of variables
      --
      -- An equality @var1 := var2@ we will regard as extending the equivalence
      -- classes of variables (see 'constructEquivClasses').
    , classifiedExtendEquivClass :: [(TcTyVar, TcTyVar)]

      -- | Substitutions we need to reconsider later
      --
      -- An equality @var1 := var2 args@ (with @args@ a non-empty list of
      -- arguments) is most problematic. Applying it /may/ allow us to make
      -- progress, but it may not (consider for example @var := var arg@). We
      -- will reconsider such equalities at the end (see 'process').
    , classifiedReconsider :: [(TcTyVar, (TcTyVar, NonEmpty Type))]
    }

instance Semigroup Classified where
  c1 <> c2 = Classified {
        classifiedProductive       = combine classifiedProductive
      , classifiedExtendEquivClass = combine classifiedExtendEquivClass
      , classifiedReconsider       = combine classifiedReconsider
      }
    where
      combine :: (Classified -> [a]) -> [a]
      combine f = f c1 ++ f c2

instance Monoid Classified where
  mempty = Classified [] [] []

productive :: TcTyVar -> (TyCon, [Type]) -> Classified
productive var (tyCon, args) = mempty {
      classifiedProductive = [(var, (tyCon, args))]
    }

extendEquivClass :: TcTyVar -> TcTyVar -> Classified
extendEquivClass var var' = mempty {
      classifiedExtendEquivClass = [(var, var')]
    }

reconsider :: TcTyVar -> (TcTyVar, NonEmpty Type) -> Classified
reconsider var (var', args) = mempty {
      classifiedReconsider = [(var, (var', args))]
    }

-- | Classify a set of given constraints
--
-- See 'Classified' for details.
classify :: [Ct] -> Classified
classify = go mempty
  where
    go :: Classified -> [Ct] -> Classified
    go acc []     = acc
    go acc (c:cs) =
        case isCanonicalVarEq c of
          Just (var, splitAppTys -> (fn, args))
            | Just tyCon <- tyConAppTyCon_maybe fn ->
                go (productive var (tyCon, args) <> acc) cs
            | Just var' <- getTyVar_maybe fn, null args ->
                go (extendEquivClass var var' <> acc) cs
            | Just var' <- getTyVar_maybe fn, x:xs <- args ->
                go (reconsider var (var', x :| xs) <> acc) cs
          _otherwise ->
            go acc cs

{-------------------------------------------------------------------------------
  Processing
-------------------------------------------------------------------------------}

-- | Construct 'TyCon' substitution from classified equality constraints
--
-- The difficult part in constructing this substitution are the equalities of
-- the form @var1 ~ var2 args@, which we ear-marked as "to reconsider" during
-- classification.
--
-- We will do this iteratively:
--
-- o We first construct a set of variable equivalence classes based on
--   'classifiedExtendEquivClass' (using 'constructEquivClasses'), and use that
--   along with the "obviously productive" equalities ('classifiedProductive')
--   as the initial value of the accumulator (a 'TyConSubst').
-- o We then repeatedly consider the remaining equalities. Whenever there is
--   a substitution available in the accumulator for @var2@ which turns it into
--   a type of the form @TyCon args'@, we add @var1 := TyCon args' args@ to the
--   accumulator.
-- o We keep doing this until we can make no more progress.
--
-- The functions for working with 'TyConSubst' take the variable equivalence
-- classes into acocunt, so we do not need to do that here.
--
-- Two observations:
--
-- o This process must terminate: there are a finite number of constraints
--   to consider, and whenever we apply a substitution from the accumulator,
--   we get an "obviously productive" substitution: we do not create new work
--   in the loop.
-- o We may end up ignoring some substitutions: if there is a substitution
--   @var1 := var2 args@ and we don't have any (productive) substitutions for
--   @var2@, we will just ignore it.
--
-- A note on recursive bindings: a direct or indirect recursive binding
--
-- > x := x args1      x := y args1
-- >                   y := x args2
--
-- where @args1, args2@ are non-empty lists of arguments, /cannot/ be relevant:
-- if they were, that would imply that there is some type constructor (regular
-- datatype or type family) which can be applied to an arbitrary number of
-- arguments. Such datatypes or type families cannot be defined in Haskell.
-- We therefore take no special care in handling recursive bindings, other than
-- to note (as we did above) that the process must terminate.
process :: Classified -> TyConSubst
process Classified{..} =
    go initSubst classifiedReconsider
  where
    initSubst :: TyConSubst
    initSubst =
          tyConSubstExtend classifiedProductive
        $ tyConSubstEmpty (constructEquivClasses classifiedExtendEquivClass)

    go :: TyConSubst
       -> [(TcTyVar, (TcTyVar, NonEmpty Type))]
       -> TyConSubst
    go acc rs =
        let (prod, rest) = tryApply makeProductive rs in
        if null prod
          then acc -- No other equations can be made productive
          else go (tyConSubstExtend prod acc) rest
      where
        makeProductive ::
             (TcTyVar, (TcTyVar, NonEmpty Type))
          -> Maybe (NonEmpty (TcTyVar, (TyCon, [Type])))
        makeProductive (var, (var', args)) =
            fmap (fmap (uncurry aux)) (tyConSubstLookup var' acc)
          where
            aux :: TyCon -> [Type] -> (TcTyVar, (TyCon, [Type]))
            aux tyCon args' = (var, (tyCon, (args' ++ toList args)))

-- | Construct a 'TyConSubst' from a collection of Given constraints.
mkTyConSubst :: [Ct] -> TyConSubst
mkTyConSubst = process . classify

{-------------------------------------------------------------------------------
  Using
-------------------------------------------------------------------------------}

-- | Like 'splitTyConApp_maybe', but taking Given constraints into account.
--
-- Looks through type synonyms, just like 'splitTyConApp_maybe' does.
splitTyConApp_upTo :: TyConSubst -> Type -> Maybe (NonEmpty (TyCon, [Type]))
splitTyConApp_upTo subst typ = asum [
      -- Direct match
      do tyCon <- tyConAppTyCon_maybe fn
         return ((tyCon, args) :| [])

      -- Indirect match
    , do var <- getTyVar_maybe fn
         fmap (fmap (second (++ args))) $ tyConSubstLookup var subst
    ]
  where
    (fn, args) = splitAppTys typ

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable TyConSubst where
  ppr TyConSubst{..} = parens $
          text "TyConSubst"
      <+> ppr tyConSubstMap
      <+> ppr tyConSubstCanon

{-------------------------------------------------------------------------------
  Canonical equalities
-------------------------------------------------------------------------------}

isCanonicalVarEq :: Ct -> Maybe (TcTyVar, Type)
isCanonicalVarEq = \case
#if __GLASGOW_HASKELL__ < 902
    CTyEqCan { cc_tyvar, cc_rhs } ->
      Just (cc_tyvar, cc_rhs)
    CFunEqCan { cc_fsk, cc_fun, cc_tyargs } ->
      Just (cc_fsk, mkTyConApp cc_fun cc_tyargs)
    _otherwise    -> Nothing
#elif __GLASGOW_HASKELL__ < 907
    CEqCan { cc_lhs, cc_rhs }
      | TyVarLHS var <- cc_lhs
      -> Just (var, cc_rhs)
      | TyFamLHS tyCon args <- cc_lhs
      , Just var            <- getTyVar_maybe cc_rhs
      -> Just (var, mkTyConApp tyCon args)
    _otherwise
      -> Nothing
#else
    CEqCan eqCt
      | TyVarLHS var <- lhs
      -> Just (var, rhs)
      | TyFamLHS tyCon args <- lhs
      , Just var            <- getTyVar_maybe rhs
      -> Just (var, mkTyConApp tyCon args)
      where
        lhs = eq_lhs eqCt
        rhs = eq_rhs eqCt
    _otherwise
      -> Nothing
#endif

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Attempt to apply a non-deterministic function to a list of values
--
-- Returns the successful results as well as the inputs on which the function
-- failed.
tryApply :: forall a b. (a -> Maybe (NonEmpty b)) -> [a] -> ([b], [a])
tryApply f = first (concat . map toList) . partitionEithers . map f'
  where
    f' :: a -> Either (NonEmpty b) a
    f' a = maybe (Right a) Left $ f a

{-------------------------------------------------------------------------------
  Equivalence classes
-------------------------------------------------------------------------------}

-- | Given a set of equivalent pairs, map every value to canonical value
--
-- Example with two classes:
--
-- >>> constructEquivClasses [(1, 2), (4, 5), (2, 3)]
-- fromList [(1,1),(2,1),(3,1),(4,4),(5,4)]
--
-- Adding one element that connects both classes:
--
-- >>> constructEquivClasses [(1, 2), (4, 5), (2, 3), (3, 4)]
-- fromList [(1,1),(2,1),(3,1),(4,1),(5,1)]
constructEquivClasses :: forall a. Ord a => [(a, a)] -> Map a a
constructEquivClasses equivs =
     Map.unions $ map (pickCanonical . map fromVertex . toList) $
       Graph.components graph
  where
    allValues :: Set a
    allValues = Set.fromList $ concatMap (\(x, y) -> [x, y]) equivs

    toVertex   :: a -> Vertex
    fromVertex :: Vertex -> a

    toVertex   a = Map.findWithDefault (error "toVertex: impossible")   a $
                     Map.fromList $ zip (Set.toList allValues) [1..]
    fromVertex v = Map.findWithDefault (error "fromVertex: impossible") v $
                     Map.fromList $ zip [1..] (Set.toList allValues)

    graph :: Graph
    graph = Graph.buildG (1, Set.size allValues) $
              map (bimap toVertex toVertex) equivs

    -- Given a previously established equivalence class, construct a mapping
    -- that maps each value to an (arbitrary) canonical value.
    pickCanonical :: [a] -> Map a a
    pickCanonical cls = Map.fromList $ zip cls (repeat (minimum cls))

canonicalize :: Ord a => Map a a -> a -> a
canonicalize canon x = Map.findWithDefault x x canon