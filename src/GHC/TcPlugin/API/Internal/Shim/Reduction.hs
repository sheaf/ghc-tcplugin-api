{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module GHC.TcPlugin.API.Internal.Shim.Reduction where

-- base
import Prelude
  hiding (Floating(cos))

-- ghc
import GHC.Core.Class
  ( Class(classTyCon) )
import GHC.Core.Coercion
  ( Coercion, CoercionN, MCoercion(..)
  , Role(Nominal), LiftingContext
#if MIN_VERSION_ghc(9,0,0)
  , castCoercionKind1, castCoercionKind2
  , coercionLKind, coercionRKind
#else
  , mkCoherenceLeftCo, mkNomReflCo
#endif
  , coercionKind
  , coToMCo
  , decomposePiCos, downgradeRole
  , liftCoSubst, emptyLiftingContext, extendLiftingContextAndInScope, zapLiftingContext
  , mkAppCo, mkAppCos
  , mkCoherenceRightCo
  , mkForAllCo, mkFunCo
  , mkGReflLeftCo, mkGReflRightCo
  , mkHomoForAllCos, mkProofIrrelCo
  , mkReflCo, mkSubCo, mkSymCo, mkTransCo, mkTyConAppCo
  )
import GHC.Core.Predicate
  ( mkClassPred )
import GHC.Core.TyCo.Rep
  ( TyCoBinder, mkFunTy
#if !MIN_VERSION_ghc(9,0,0)
  , Coercion(..)
#endif
  )
import GHC.Core.TyCon
  ( TyCon )
import GHC.Core.Type
  ( AnonArgFlag, ArgFlag, Kind, Type, TyVar, TyVarBinder
  , binderVars
  , mkAppTy, mkAppTys, mkCastTy, mkCoercionTy, mkForAllTy, mkForAllTys
  , mkTyConApp, mkPiTys
  , noFreeVarsOfType
  , splitPiTys, tyCoBinderType, tyCoBinderVar_maybe
  )
import GHC.Data.Pair
  ( Pair(Pair) )
import GHC.Types.Var
  ( setTyVarKind )
import GHC.Types.Var.Env
  ( mkInScopeSet )
import GHC.Types.Var.Set
  ( TyCoVarSet )
import GHC.Utils.Outputable
  ( Outputable(ppr), (<+>)
  , braces, text, vcat
  )

--------------------------------------------------------------------------------


-- | A 'Reduction' is the result of an operation that rewrites a type @ty_in@.
-- The 'Reduction' includes the rewritten type @ty_out@ and a 'Coercion' @co@
-- such that @co :: ty_in ~ ty_out@, where the role of the coercion is determined
-- by the context. That is, the LHS type of the coercion is the original type
-- @ty_in@, while its RHS type is the rewritten type @ty_out@.
--
-- A Reduction is always homogeneous, unless it is wrapped inside a 'HetReduction',
-- which separately stores the kind coercion.
data Reduction =
  Reduction
    { reductionCoercion    :: Coercion
    , reductionReducedType :: !Type
    }

-- | Stores a heterogeneous reduction.
--
-- The stored kind coercion must relate the kinds of the
-- stored reduction. That is, in @HetReduction (Reduction co xi) kco@,
-- we must have:
--
-- >  co :: ty ~ xi
-- > kco :: typeKind ty ~ typeKind xi
data HetReduction =
  HetReduction
    Reduction
    MCoercion

-- | Create a heterogeneous reduction.
--
-- Pre-condition: the provided kind coercion (second argument)
-- relates the kinds of the stored reduction.
-- That is, if the coercion stored in the 'Reduction' is of the form
--
-- > co :: ty ~ xi
--
-- Then the kind coercion supplied must be of the form:
--
-- > kco :: typeKind ty ~ typeKind xi
mkHetReduction :: Reduction  -- ^ heterogeneous reduction
               -> MCoercion  -- ^ kind coercion
               -> HetReduction
mkHetReduction redn mco = HetReduction redn mco
{-# INLINE mkHetReduction #-}

-- | Homogenise a heterogeneous reduction.
--
-- Given @HetReduction (Reduction co xi) kco@, with
--
-- >  co :: ty ~ xi
-- > kco :: typeKind(ty) ~ typeKind(xi)
--
-- this returns the homogeneous reduction:
--
-- > hco :: ty ~ ( xi |> sym kco )
homogeniseHetRedn :: Role -> HetReduction -> Reduction
homogeniseHetRedn role (HetReduction redn kco)
  = mkCoherenceRightMRedn role redn (mkSymMCo kco)
{-# INLINE homogeniseHetRedn #-}

-- | Create a 'Reduction' from a pair of a 'Coercion' and a 'Type.
--
-- Pre-condition: the RHS type of the coercion matches the provided type
-- (perhaps up to zonking).
--
-- Use 'coercionRedn' when you only have the coercion.
mkReduction :: Coercion -> Type -> Reduction
mkReduction co ty = Reduction co ty
{-# INLINE mkReduction #-}

instance Outputable Reduction where
  ppr redn =
    braces $ vcat
      [ text "reductionOriginalType:" <+> ppr (reductionOriginalType redn)
      , text " reductionReducedType:" <+> ppr (reductionReducedType redn)
      , text "    reductionCoercion:" <+> ppr (reductionCoercion redn)
      ]

-- | A 'Reduction' in which the 'Coercion' has 'Nominal' role.
type ReductionN = Reduction

-- | A 'Reduction' in which the 'Coercion' has 'Representational' role.
type ReductionR = Reduction

-- | Get the original, unreduced type corresponding to a 'Reduction'.
--
-- This is obtained by computing the LHS kind of the stored coercion,
-- which may be slow.
reductionOriginalType :: Reduction -> Type
reductionOriginalType = coercionLKind . reductionCoercion
{-# INLINE reductionOriginalType #-}

-- | Turn a 'Coercion' into a 'Reduction'
-- by inspecting the RHS type of the coercion.
--
-- Prefer using 'mkReduction' when you already know
-- the RHS type of the coercion, to avoid computing it anew.
coercionRedn :: Coercion -> Reduction
coercionRedn co = Reduction co (coercionRKind co)
{-# INLINE coercionRedn #-}

-- | Downgrade the role of the coercion stored in the 'Reduction'.
downgradeRedn :: Role -- ^ desired role
              -> Role -- ^ current role
              -> Reduction
              -> Reduction
downgradeRedn new_role old_role redn@(Reduction co _)
  = redn { reductionCoercion = downgradeRole new_role old_role co }
{-# INLINE downgradeRedn #-}

-- | Downgrade the role of the coercion stored in the 'Reduction',
-- from 'Nominal' to 'Representational'.
mkSubRedn :: Reduction -> Reduction
mkSubRedn redn@(Reduction co _) = redn { reductionCoercion = mkSubCo co }
{-# INLINE mkSubRedn #-}

-- | Compose a reduction with a coercion on the left.
--
-- Pre-condition: the provided coercion's RHS type must match the LHS type
-- of the coercion that is stored in the reduction.
mkTransRedn :: Coercion -> Reduction -> Reduction
mkTransRedn co1 redn@(Reduction co2 _)
  = redn { reductionCoercion = co1 `mkTransCo` co2 }
{-# INLINE mkTransRedn #-}

-- | The reflexive reduction.
mkReflRedn :: Role -> Type -> Reduction
mkReflRedn r ty = mkReduction (mkReflCo r ty) ty

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the rewritten type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @ty ~res_co~> (ty |> mco)@
-- at the given 'Role'.
mkGReflRightRedn :: Role -> Type -> CoercionN -> Reduction
mkGReflRightRedn role ty co
  = mkReduction
      (mkGReflRightCo role ty co)
      (mkCastTy ty co)
{-# INLINE mkGReflRightRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the rewritten type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @ty ~res_co~> (ty |> mco)@
-- at the given 'Role'.
mkGReflRightMRedn :: Role -> Type -> MCoercion -> Reduction
mkGReflRightMRedn role ty mco
  = mkReduction
      (mkGReflRightMCo role ty mco)
      (mkCastTyMCo ty mco)
{-# INLINE mkGReflRightMRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the original (non-rewritten) type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @(ty |> mco) ~res_co~> ty@
-- at the given 'Role'.
mkGReflLeftRedn :: Role -> Type -> CoercionN -> Reduction
mkGReflLeftRedn role ty co
  = mkReduction
      (mkGReflLeftCo role ty co)
      ty
{-# INLINE mkGReflLeftRedn #-}

-- | Create a 'Reduction' from a kind cast, in which
-- the casted type is the original (non-rewritten) type.
--
-- Given @ty :: k1@, @mco :: k1 ~ k2@,
-- produces the 'Reduction' @(ty |> mco) ~res_co~> ty@
-- at the given 'Role'.
mkGReflLeftMRedn :: Role -> Type -> MCoercion -> Reduction
mkGReflLeftMRedn role ty mco
  = mkReduction
      (mkGReflLeftMCo role ty mco)
      ty
{-# INLINE mkGReflLeftMRedn #-}

-- | Apply a cast to the result of a 'Reduction'.
--
-- Given a 'Reduction' @ty1 ~co1~> (ty2 :: k2)@ and a kind coercion @kco@
-- with LHS kind @k2@, produce a new 'Reduction' @ty1 ~co2~> ( ty2 |> kco )@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
mkCoherenceRightRedn :: Role -> Reduction -> CoercionN -> Reduction
mkCoherenceRightRedn r (Reduction co1 ty2) kco
  = mkReduction
      (mkCoherenceRightCo r ty2 kco co1)
      (mkCastTy ty2 kco)
{-# INLINE mkCoherenceRightRedn #-}

-- | Apply a cast to the result of a 'Reduction', using an 'MCoercionN'.
--
-- Given a 'Reduction' @ty1 ~co1~> (ty2 :: k2)@ and a kind coercion @mco@
-- with LHS kind @k2@, produce a new 'Reduction' @ty1 ~co2~> ( ty2 |> mco )@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
mkCoherenceRightMRedn :: Role -> Reduction -> MCoercion -> Reduction
mkCoherenceRightMRedn r (Reduction co1 ty2) kco
  = mkReduction
      (mkCoherenceRightMCo r ty2 kco co1)
      (mkCastTyMCo ty2 kco)
{-# INLINE mkCoherenceRightMRedn #-}

-- | Apply a cast to a 'Reduction', casting both the original and the reduced type.
--
-- Given @cast_co@ and 'Reduction' @ty ~co~> xi@, this function returns
-- the 'Reduction' @(ty |> cast_co) ~return_co~> (xi |> cast_co)@
-- of the given 'Role' (which must match the role of the coercion stored
-- in the 'Reduction' argument).
--
-- Pre-condition: the 'Type' passed in is the same as the LHS type
-- of the coercion stored in the 'Reduction'.
mkCastRedn1 :: Role
            -> Type      -- ^ original type
            -> CoercionN -- ^ coercion to cast with
            -> Reduction -- ^ rewritten type, with rewriting coercion
            -> Reduction
mkCastRedn1 r ty cast_co (Reduction co xi)
  -- co :: ty ~r ty'
  -- return_co :: (ty |> cast_co) ~r (ty' |> cast_co)
  = mkReduction
      (castCoercionKind1 co r ty xi cast_co)
      (mkCastTy xi cast_co)
{-# INLINE mkCastRedn1 #-}

-- | Apply casts on both sides of a 'Reduction' (of the given 'Role').
--
-- Use 'mkCastRedn1' when you want to cast both the original and reduced types
-- in a 'Reduction' using the same coercion.
--
-- Pre-condition: the 'Type' passed in is the same as the LHS type
-- of the coercion stored in the 'Reduction'.
mkCastRedn2 :: Role
            -> Type      -- ^ original type
            -> CoercionN -- ^ coercion to cast with on the left
            -> Reduction -- ^ rewritten type, with rewriting coercion
            -> CoercionN -- ^ coercion to cast with on the right
            -> Reduction
mkCastRedn2 r ty cast_co (Reduction nco nty) cast_co'
  = mkReduction
      (castCoercionKind2 nco r ty nty cast_co cast_co')
      (mkCastTy nty cast_co')
{-# INLINE mkCastRedn2 #-}

-- | Apply one 'Reduction' to another.
--
-- Combines 'mkAppCo' and 'mkAppTy`.
mkAppRedn :: Reduction -> Reduction -> Reduction
mkAppRedn (Reduction co1 ty1) (Reduction co2 ty2)
  = mkReduction (mkAppCo co1 co2) (mkAppTy ty1 ty2)
{-# INLINE mkAppRedn #-}

-- | Create a function 'Reduction'.
--
-- Combines 'mkFunCo' and 'mkFunTy'.
mkFunRedn :: Role
          -> AnonArgFlag
#if MIN_VERSION_ghc(9,0,0)
          -> ReductionN -- ^ multiplicity reduction
#endif
          -> Reduction  -- ^ argument reduction
          -> Reduction  -- ^ result reduction
          -> Reduction
mkFunRedn r vis
#if MIN_VERSION_ghc(9,0,0)
  (Reduction w_co w_ty)
#endif
  (Reduction arg_co arg_ty)
  (Reduction res_co res_ty)
    = mkReduction
        ( mkFunCo
            r
#if MIN_VERSION_ghc(9,0,0)
            w_co
#endif
            arg_co
            res_co
        )
        ( mkFunTy
            vis
#if MIN_VERSION_ghc(9,0,0)
            w_ty
#endif
            arg_ty
            res_ty
        )
{-# INLINE mkFunRedn #-}

-- | Create a 'Reduction' associated to a Π type,
-- from a kind 'Reduction' and a body 'Reduction'.
--
-- Combines 'mkForAllCo' and 'mkForAllTy'.
mkForAllRedn :: ArgFlag
             -> TyVar
             -> ReductionN -- ^ kind reduction
             -> Reduction  -- ^ body reduction
             -> Reduction
mkForAllRedn vis tv1 (Reduction h ki') (Reduction co ty)
  = mkReduction
      (mkForAllCo tv1 h co)
      (mkForAllTy tv2 vis ty)
  where
    tv2 = setTyVarKind tv1 ki'
{-# INLINE mkForAllRedn #-}

-- | Create a 'Reduction' of a quantified type from a
-- 'Reduction' of the body.
--
-- Combines 'mkHomoForAllCos' and 'mkForAllTys'.
mkHomoForAllRedn :: [TyVarBinder] -> Reduction -> Reduction
mkHomoForAllRedn bndrs (Reduction co ty)
  = mkReduction
      (mkHomoForAllCos (binderVars bndrs) co)
      (mkForAllTys bndrs ty)
{-# INLINE mkHomoForAllRedn #-}

-- | Create a 'Reduction' from a coercion between coercions.
--
-- Combines 'mkProofIrrelCo' and 'mkCoercionTy'.
mkProofIrrelRedn :: Role      -- ^ role of the created coercion, "r"
                 -> CoercionN -- ^ co :: phi1 ~N phi2
                 -> Coercion  -- ^ g1 :: phi1
                 -> Coercion  -- ^ g2 :: phi2
                 -> Reduction -- ^ res_co :: g1 ~r g2
mkProofIrrelRedn role co g1 g2
  = mkReduction
      (mkProofIrrelCo role co g1 g2)
      (mkCoercionTy g2)
{-# INLINE mkProofIrrelRedn #-}

-- | Create a reflexive 'Reduction' whose RHS is the given 'Coercion',
-- with the specified 'Role'.
mkReflCoRedn :: Role -> Coercion -> Reduction
mkReflCoRedn role co
  = mkReduction
      (mkReflCo role co_ty)
      co_ty
  where
    co_ty = mkCoercionTy co
{-# INLINE mkReflCoRedn #-}

-- | A collection of 'Reduction's where the coercions and the types are stored separately.
--
-- Use 'unzipRedns' to obtain 'Reductions' from a list of 'Reduction's.
--
-- This datatype is used in 'mkAppRedns', 'mkClassPredRedns' and 'mkTyConAppRedn',
-- which expect separate types and coercions.
--
-- Invariant: the two stored lists are of the same length,
-- and the RHS type of each coercion is the corresponding type.
data Reductions = Reductions [Coercion] [Type]

-- | Create 'Reductions' from individual lists of coercions and types.
--
-- The lists should be of the same length, and the RHS type of each coercion
-- should match the specified type in the other list.
mkReductions :: [Coercion] -> [Type] -> Reductions
mkReductions cos tys = Reductions cos tys
{-# INLINE mkReductions #-}

-- | Combines 'mkAppCos' and 'mkAppTys'.
mkAppRedns :: Reduction -> Reductions -> Reduction
mkAppRedns (Reduction co ty) (Reductions cos tys)
  = mkReduction (mkAppCos co cos) (mkAppTys ty tys)
{-# INLINE mkAppRedns #-}

-- | 'TyConAppCo' for 'Reduction's: combines 'mkTyConAppCo' and `mkTyConApp`.
mkTyConAppRedn :: Role -> TyCon -> Reductions -> Reduction
mkTyConAppRedn role tc (Reductions cos tys)
  = mkReduction (mkTyConAppCo role tc cos) (mkTyConApp tc tys)
{-# INLINE mkTyConAppRedn #-}

-- | Reduce the arguments of a 'Class' 'TyCon'.
mkClassPredRedn :: Class -> Reductions -> Reduction
mkClassPredRedn cls (Reductions cos tys)
  = mkReduction
      (mkTyConAppCo Nominal (classTyCon cls) cos)
      (mkClassPred cls tys)
{-# INLINE mkClassPredRedn #-}

-- | Obtain 'Reductions' from a list of 'Reduction's by unzipping.
unzipRedns :: [Reduction] -> Reductions
unzipRedns = foldr accRedn (Reductions [] [])
  where
    accRedn :: Reduction -> Reductions -> Reductions
    accRedn (Reduction co xi) (Reductions cos xis)
      = Reductions (co:cos) (xi:xis)
{-# INLINE unzipRedns #-}

--------------------------------------------------------------------------------

data ArgsReductions =
  ArgsReductions
    {-# UNPACK #-} !Reductions
    !MCoercion

{-# INLINE simplifyArgsWorker #-}
simplifyArgsWorker :: [TyCoBinder] -> Kind -> TyCoVarSet -> [Role] -> [Reduction] -> ArgsReductions
simplifyArgsWorker orig_ki_binders orig_inner_ki orig_fvs
                   orig_roles orig_simplified_args
  = go orig_lc
       orig_ki_binders orig_inner_ki
       orig_roles orig_simplified_args
  where
    orig_lc = emptyLiftingContext $ mkInScopeSet $ orig_fvs

    go :: LiftingContext -> [TyCoBinder] -> Kind -> [Role] -> [Reduction] -> ArgsReductions
    go !lc binders inner_ki _ []
      = ArgsReductions
          (mkReductions [] [])
          kind_co
      where
        final_kind = mkPiTys binders inner_ki
        kind_co | noFreeVarsOfType final_kind = MRefl
                | otherwise                   = MCo $ liftCoSubst Nominal lc final_kind

    go lc (binder:binders) inner_ki (role:roles) (arg_redn:arg_redns)
      =  let !kind_co = liftCoSubst Nominal lc (tyCoBinderType binder)
             !(Reduction casted_co casted_xi)
                      = mkCoherenceRightRedn role arg_redn kind_co
         -- now, extend the lifting context with the new binding
             !new_lc | Just tv <- tyCoBinderVar_maybe binder
                     = extendLiftingContextAndInScope lc tv casted_co
                     | otherwise
                     = lc
             !(ArgsReductions (Reductions cos xis) final_kind_co)
               = go new_lc binders inner_ki roles arg_redns
         in ArgsReductions
              (Reductions (casted_co:cos) (casted_xi:xis))
              final_kind_co

    -- See Note [Last case in simplifyArgsWorker]
    go lc [] inner_ki roles arg_redns
      = let co1 = liftCoSubst Nominal lc inner_ki
            co1_kind              = coercionKind co1
            unrewritten_tys       = map reductionOriginalType arg_redns
            (arg_cos, res_co)     = decomposePiCos co1 co1_kind unrewritten_tys
            casted_args           = zipWith3 mkCoherenceRightRedn roles arg_redns arg_cos
            zapped_lc             = zapLiftingContext lc
            Pair rewritten_kind _ = co1_kind
            (bndrs, new_inner)    = splitPiTys rewritten_kind

            ArgsReductions redns_out res_co_out
              = go zapped_lc bndrs new_inner roles casted_args
        in
          ArgsReductions redns_out (res_co `mkTransMCoR` res_co_out)

    go _ _ _ _ _ = error "simplifyArgsWorker wandered into deeper water than usual"

--------------------------------------------------------------------------------

-- | Get the reverse of an 'MCoercion'
mkSymMCo :: MCoercion -> MCoercion
mkSymMCo MRefl    = MRefl
mkSymMCo (MCo co) = MCo (mkSymCo co)

mkGReflLeftMCo :: Role -> Type -> MCoercion -> Coercion
mkGReflLeftMCo r ty MRefl    = mkReflCo r ty
mkGReflLeftMCo r ty (MCo co) = mkGReflLeftCo r ty co

mkGReflRightMCo :: Role -> Type -> MCoercion -> Coercion
mkGReflRightMCo r ty MRefl    = mkReflCo r ty
mkGReflRightMCo r ty (MCo co) = mkGReflRightCo r ty co

-- | Cast a type by an 'MCoercion'
mkCastTyMCo :: Type -> MCoercion -> Type
mkCastTyMCo ty MRefl    = ty
mkCastTyMCo ty (MCo co) = ty `mkCastTy` co

-- | Like 'mkCoherenceRightCo', but with an 'MCoercion'
mkCoherenceRightMCo :: Role -> Type -> MCoercion -> Coercion -> Coercion
mkCoherenceRightMCo _ _  MRefl    co2 = co2
mkCoherenceRightMCo r ty (MCo co) co2 = mkCoherenceRightCo r ty co co2

mkTransMCoR :: Coercion -> MCoercion -> MCoercion
mkTransMCoR co1 MRefl     = coToMCo co1
mkTransMCoR co1 (MCo co2) = MCo (mkTransCo co1 co2)

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,0,0)

coercionLKind, coercionRKind :: Coercion -> Type
coercionLKind co = case coercionKind co of { Pair lco _ -> lco }
coercionRKind co = case coercionKind co of { Pair _ rco -> rco }

-- | Creates a new coercion with both of its types casted by different casts
-- @castCoercionKind2 g r t1 t2 h1 h2@, where @g :: t1 ~r t2@,
-- has type @(t1 |> h1) ~r (t2 |> h2)@.
-- @h1@ and @h2@ must be nominal.
castCoercionKind2 :: Coercion -> Role -> Type -> Type
                 -> CoercionN -> CoercionN -> Coercion
castCoercionKind2 g r t1 t2 h1 h2
  = mkCoherenceRightCo r t2 h2 (mkCoherenceLeftCo r t1 h1 g)

-- | @castCoercionKind1 g r t1 t2 h@ = @coercionKind g r t1 t2 h h@
-- That is, it's a specialised form of castCoercionKind, where the two
--          kind coercions are identical
-- @castCoercionKind1 g r t1 t2 h@, where @g :: t1 ~r t2@,
-- has type @(t1 |> h) ~r (t2 |> h)@.
-- @h@ must be nominal.
-- See Note [castCoercionKind1]
castCoercionKind1 :: Coercion -> Role -> Type -> Type
                  -> CoercionN -> Coercion
castCoercionKind1 g r t1 t2 h
  = case g of
      Refl {} -> mkNomReflCo (mkCastTy t2 h)
      GRefl _ _ mco -> case mco of
           MRefl       -> mkReflCo r (mkCastTy t2 h)
           MCo kind_co -> GRefl r (mkCastTy t1 h) $
                          MCo (mkSymCo h `mkTransCo` kind_co `mkTransCo` h)
      _ -> castCoercionKind2 g r t1 t2 h h
#endif
