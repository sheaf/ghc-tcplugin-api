{-# LANGUAGE CPP #-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module: GHC.TcPlugin.API.TyConSubst

This module provides functionality for recognising whether a type is a
'TyConApp' while taking into account Given nominal equalities.

In particular, this allows dealing with flattening variables on older GHC versions
(9.0 and below). For example, instead of @[W] m + n ~ n + m@, older GHCs might
produce @[G] u ~ m + n, [G] v ~ n + m, [W] u ~ v@. In this case, one needs to
use the Givens to recognise that @u ~ v@ can be solved using the laws of natural
number arithmetic.

Usage:

  - Use 'mkTyConSubst' to create a 'TyConSubst' from Givens.
  - Use 'splitTyConApp_upTo' to compute whether a type is a 'TyConApp', taking
    into account Given constraints (in the form of the 'TyConSubst').

Notes:

  - 'splitTyConApp_upTo' will also look through type synonyms,
  - 'splitTyConApp_upTo' only takes into account nominal equalities.
    Please open a ticket if you have a need for rewriting modulo representational
    equalities.

-}
module GHC.TcPlugin.API.TyConSubst (
    TyConSubst -- opaque
  , mkTyConSubst
  , splitTyConApp_upTo
  ) where

-- Word64Map is available:
--
--  - from 9.10.1 onwards
--  - in 9.8.4
--  - in 9.6.7
--
-- This is unusual: whether a module is exposed depends on the MINOR version
#define HAS_WORD64MAP \
    (MIN_VERSION_ghc(9,9,0) || \
    (MIN_VERSION_ghc(9,8,4) && !(MIN_VERSION_ghc(9,9,0))) || \
    (MIN_VERSION_ghc(9,6,7) && !(MIN_VERSION_ghc(9,7,0))))

-- base
import Data.Bifunctor
  ( Bifunctor(first, second) )
import Data.Either
  ( partitionEithers )
import Data.Foldable
  ( toList, asum )
import Data.List.NonEmpty
  ( NonEmpty(..) )
import Data.Maybe
  ( fromJust )
#if !MIN_VERSION_ghc(8,11,0)
import Unsafe.Coerce
  ( unsafeCoerce )
#endif

-- containers
import Data.Sequence
  ( Seq )
import qualified Data.Sequence as Seq

-- ghc
#if HAS_WORD64MAP
import qualified GHC.Data.Word64Map.Strict as Word64Map
#else
import qualified Data.IntMap.Strict as IntMap
#endif
import GHC.Types.Unique
  ( Uniquable, getUnique )
import qualified GHC.Types.Unique.FM as UFM
import GHC.Types.Unique.Set
  ( UniqSet )
import qualified GHC.Types.Unique.Set as UniqSet
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

-- | Substitution for recognizing 'TyCon' applications modulo nominal equalities.
data TyConSubst = TyConSubst {
      tyConSubstMap :: UniqFM TcTyVar (NonEmpty (TyCon, [Type], [Coercion]))
    , tyConSubstCanon :: UniqFM TcTyVar (TcTyVar, [Coercion])
    }
-- During constraint solving the set of Given constraints includes so-called
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
--   try to apply the canonical equalities as often as is necessary (e.g.,
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

{-------------------------------------------------------------------------------
  Basic functionality for working with 'TyConSubst'
-------------------------------------------------------------------------------}

-- | Empty substitution
--
-- The canonical variables map is established once when the initial substitution
-- is generated and not updated thereafter.
tyConSubstEmpty :: UniqFM TcTyVar (TcTyVar, [Coercion]) -> TyConSubst
tyConSubstEmpty canon = TyConSubst {
      tyConSubstMap   = UFM.emptyUFM
    , tyConSubstCanon = canon
    }

-- | Lookup a variable in the substitution
tyConSubstLookup :: TcTyVar -> TyConSubst -> Maybe (NonEmpty (TyCon, [Type], [Coercion]))
tyConSubstLookup var TyConSubst{..} =
  fmap ( \ (tc, tys, deps) -> (tc, tys, deps ++ deps')) <$> UFM.lookupUFM tyConSubstMap var'
  where
    var' :: TcTyVar
    deps' :: [Coercion]
    (var', deps') = canonicalize tyConSubstCanon var

-- | Extend substitution with new bindings
tyConSubstExtend ::
     [(TcTyVar, (TyCon, [Type]), [Coercion])]
  -> TyConSubst -> TyConSubst
tyConSubstExtend new subst@TyConSubst{..} = subst {
      tyConSubstMap = UFM.plusUFM_C (<>)
                        (UFM.listToUFM $ map aux new)
                        tyConSubstMap
    }
  where
    aux :: (TcTyVar,          (TyCon, [Type]), [Coercion])
        -> (TcTyVar, NonEmpty (TyCon, [Type] , [Coercion]))
    aux (var, (tc, args), deps) =
      let (var', deps') = canonicalize tyConSubstCanon var
      in  (var', (tc, args, deps ++ deps') :| [])

{-------------------------------------------------------------------------------
  Classification
-------------------------------------------------------------------------------}

-- | Classified canonical nominal equality constraints.
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
      classifiedProductive :: [(TcTyVar, (TyCon, [Type]), [Coercion])]

      -- | Extend equivalence class of variables
      --
      -- An equality @var1 := var2@ we will regard as extending the equivalence
      -- classes of variables (see 'constructEquivClasses').
    , classifiedExtendEquivClass :: [(TcTyVar, TcTyVar, [Coercion])]

      -- | Substitutions we need to reconsider later
      --
      -- An equality @var1 := var2 args@ (with @args@ a non-empty list of
      -- arguments) is most problematic. Applying it /may/ allow us to make
      -- progress, but it may not (consider for example @var := var arg@). We
      -- will reconsider such equalities at the end (see 'process').
    , classifiedReconsider :: [(TcTyVar, (TcTyVar, NonEmpty Type), [Coercion])]
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

productive :: TcTyVar -> (TyCon, [Type]) -> [Coercion] -> Classified
productive var app deps = mempty {
      classifiedProductive = [(var, app, deps)]
    }

extendEquivClass :: TcTyVar -> TcTyVar -> [Coercion] -> Classified
extendEquivClass var var' deps = mempty {
      classifiedExtendEquivClass = [(var, var', deps)]
    }

reconsider :: TcTyVar -> (TcTyVar, NonEmpty Type) -> [Coercion] -> Classified
reconsider var (var', args) deps = mempty {
      classifiedReconsider = [(var, (var', args), deps)]
    }

-- | Classify a set of Given constraints.
--
-- See 'Classified' for details.
classify :: [Ct] -> Classified
classify = go mempty
  where
    go :: Classified -> [Ct] -> Classified
    go acc []     = acc
    go acc (c:cs) =
      let deps = [ctEvCoercion (ctEvidence c)] in
        case isCanonicalVarEq c of
          Just (var, splitAppTys -> (fn, args), NomEq)
            | Just (tyCon, inner) <- splitTyConApp_maybe fn ->
                go (productive var (tyCon, inner ++ args) deps <> acc) cs
            | Just var' <- getTyVar_maybe fn, null args ->
                go (extendEquivClass var var' deps <> acc) cs
            | Just var' <- getTyVar_maybe fn, x:xs <- args ->
                go (reconsider var (var', x :| xs) deps <> acc) cs
          _otherwise ->
            go acc cs

{-------------------------------------------------------------------------------
  Processing
-------------------------------------------------------------------------------}

-- | Construct 'TyCon' substitution from classified nominal equality constraints.
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
-- classes into account, so we do not need to do that here.
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
       -> [(TcTyVar, (TcTyVar, NonEmpty Type), [Coercion])]
       -> TyConSubst
    go acc rs =
        let (prod, rest) = tryApply makeProductive rs in
        if null prod
          then acc -- No other equations can be made productive
          else go (tyConSubstExtend prod acc) rest
      where
        makeProductive ::
             (TcTyVar, (TcTyVar, NonEmpty Type), [Coercion])
          -> Maybe (NonEmpty (TcTyVar, (TyCon, [Type]), [Coercion]))
        makeProductive (var, (var', args), deps) = do
          tcApp <- tyConSubstLookup var' acc
          return $ fmap aux tcApp
          where
            aux :: (TyCon, [Type], [Coercion]) -> (TcTyVar, (TyCon, [Type]), [Coercion])
            aux (tyCon, args', deps') =
              (var, (tyCon, args' ++ toList args), deps ++ deps')

-- | Construct a 'TyConSubst' from a collection of Given constraints.
mkTyConSubst :: [Ct] -> TyConSubst
mkTyConSubst = process . classify

{-------------------------------------------------------------------------------
  Using
-------------------------------------------------------------------------------}

-- | Like 'splitTyConApp_maybe', but taking Given constraints into account.
--
-- Alongside the @TyCon@ and its arguments, also returns a list of coercions
-- that embody the Givens that we depended on.
--
-- Looks through type synonyms, just like 'splitTyConApp_maybe' does.
splitTyConApp_upTo :: TyConSubst -> Type -> Maybe (NonEmpty (TyCon, [Type], [Coercion]))
splitTyConApp_upTo subst typ = asum [
      -- Direct match
      do (tyCon, inner) <- splitTyConApp_maybe fn
         return ((tyCon, inner ++ args, []) :| [])

      -- Indirect match
    , do var <- getTyVar_maybe fn
         tcApps <- tyConSubstLookup var subst
         return $
           fmap (\ (tc, inner, deps) -> (tc, inner ++ args, deps)) tcApps
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

isCanonicalVarEq :: Ct -> Maybe (TcTyVar, Type, EqRel)
isCanonicalVarEq = \case
#if __GLASGOW_HASKELL__ < 902
    CTyEqCan { cc_tyvar, cc_rhs, cc_eq_rel } ->
      Just (cc_tyvar, cc_rhs, cc_eq_rel)
    CFunEqCan { cc_fsk, cc_fun, cc_tyargs } ->
      Just (cc_fsk, mkTyConApp cc_fun cc_tyargs, NomEq)
    _otherwise    -> Nothing
#elif __GLASGOW_HASKELL__ < 907
    CEqCan { cc_lhs, cc_rhs, cc_eq_rel }
      | TyVarLHS var <- cc_lhs
      -> Just (var, cc_rhs, cc_eq_rel)
      | TyFamLHS tyCon args <- cc_lhs
      , Just var            <- getTyVar_maybe cc_rhs
      -> Just (var, mkTyConApp tyCon args, NomEq)
    _otherwise
      -> Nothing
#else
    CEqCan eqCt
      | TyVarLHS var <- lhs
      -> Just (var, rhs, rel)
      | TyFamLHS tyCon args <- lhs
      , Just var            <- getTyVar_maybe rhs
      -> Just (var, mkTyConApp tyCon args, rel)
      where
        lhs = eq_lhs eqCt
        rhs = eq_rhs eqCt
        rel = eq_eq_rel eqCt
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

-- | Given a set of labelled equivalent pairs, map every value to a canonical
-- value in the same equivalence class, with the shortest path (as a list of
-- labels) connecting the value to the canonical value.
--
-- Example with two equivalence classes:
--
-- >>> constructEquivClasses [(1, 2, ["12"]), (4, 5, ["45"]), (2, 3, ["23"])]
-- fromList [(1,(1,[])),(2,(1,["12"])),(3,(1,["23","12"])),(4,(4,[])),(5,(4,["45"]))]
--
-- Adding one element that connects both equivalence classes:
--
-- >>> constructEquivClasses [(1, 2, ["12"]), (4, 5, ["45"]), (2, 3, ["23"]), (3, 4, ["34"])]
-- fromList [(1,(1,[])),(2,(1,["12"])),(3,(1,["23","12"])),(4,(1,["34","23","12"])),(5,(1,["45","34","23","12"]))]
constructEquivClasses :: forall a l. (Uniquable a, Monoid l) => [(a, a, l)] -> UniqFM a (a, l)
constructEquivClasses equivs = canonicals
  where
    neighbours :: UniqFM a (UniqFM a l)
    neighbours = neighboursMap equivs

    allValues :: UniqSet a
    allValues = UniqSet.mkUniqSet $ concat [ [x,y] | (x,y,_) <- equivs ]

    canonicals :: UniqFM a (a, l)
    canonicals = go UFM.emptyUFM allValues
      where
        go :: UniqFM a (a, l) -> UniqSet a -> UniqFM a (a, l)
        go acc vs =
          case minViewUniqSet vs of
            Nothing -> acc
            Just (v, vs') ->
              let
                !comp = doComp
                          ( UFM.unitUFM v mempty )
                          ( Seq.singleton $ getUnique v )
              in
                go
                  ( UFM.plusUFM acc ( ( v , ) <$> comp ) )
                  ( vs' `UniqSet.uniqSetMinusUFM` comp )

        doComp :: UniqFM a l -> Seq Unique -> UniqFM a l
        doComp !ds Seq.Empty = ds
        doComp  ds (v Seq.:<| vs) =
          let
            -- unvisited neighbours
            !us = ( fromJust $ UFM.lookupUFM_Directly neighbours v ) `UFM.minusUFM` ds
            !d = fromJust $ UFM.lookupUFM_Directly ds v

            !ds' = UFM.plusUFM ds ( UFM.mapUFM (<> d) us )
            !vs' = vs Seq.>< Seq.fromList ( UFM.nonDetKeysUFM us )
          in
            doComp ds' vs'

minViewUniqSet :: forall a. UniqSet a -> Maybe (a, UniqSet a)
minViewUniqSet s =
  let m = UFM.ufmToIntMap $ UniqSet.getUniqSet s
  in second
        ( UniqSet.unsafeUFMToUniqSet
            .
#if MIN_VERSION_ghc(8,11,0)
          UFM.unsafeIntMapToUFM
#else
          ( unsafeCoerce :: IntMap.IntMap a -> UniqFM a a )
#endif
        ) <$>
#if HAS_WORD64MAP
      Word64Map.minView
#else
      IntMap.minView
#endif
        m

neighboursMap :: forall a l. Uniquable a => [(a, a, l)] -> UniqFM a (UniqFM a l)
neighboursMap edges = foldr addEdge UFM.emptyUFM edges
  where
    addEdge :: (a, a, l) -> UniqFM a (UniqFM a l) -> UniqFM a (UniqFM a l)
    addEdge (u, v, l) m
      = UFM.addToUFM_C UFM.plusUFM
          (UFM.addToUFM_C UFM.plusUFM m v (UFM.unitUFM u l))
          u (UFM.unitUFM v l)

canonicalize :: (Uniquable a, Monoid l) => UniqFM a (a, l) -> a -> (a, l)
canonicalize canon x = UFM.lookupWithDefaultUFM canon (x, mempty) x
