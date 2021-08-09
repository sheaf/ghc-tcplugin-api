{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

{-|
Module: GHC.TcPlugin.API.Internal.Shim

This module defines a compatibility shim which allows
the library to support a limited form of type-family rewriting
in typechecking plugins on GHC 9.0 and 9.2.
-}

module GHC.TcPlugin.API.Internal.Shim
  ( Reduction(..), mkReduction
  , TcPluginSolveResult, TcPluginRewriteResult(..)
  , RewriteEnv(..)
  , shimRewriter
  )
  where

-- base
import Prelude
  hiding ( Floating(cos), iterate )
import Control.Monad
  ( forM, when )
#if !MIN_VERSION_ghc(9,2,0)
import Data.Foldable
  ( foldlM )
#endif
#if MIN_VERSION_ghc(9,2,0)
import Data.List.NonEmpty
  ( NonEmpty((:|)) )
#endif
import Data.Maybe
  ( fromMaybe )

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT(..) )
import Control.Monad.Trans.State.Strict
  ( StateT(..) )

-- ghc
import GHC.Core.Coercion
  ( mkReflCo, mkSymCo
  , mkAppCos, mkNomReflCo, mkSubCo
  , mkTyConAppCo, tyConRolesX
  , tyConRolesRepresentational
  )
import GHC.Core.Predicate
  ( EqRel(..), eqRelRole )
import GHC.Core.TyCo.Rep
  ( Type(..), Kind, Coercion(..), MCoercion(..), TyCoBinder(..)
  , isNamedBinder, mkTyVarTy
  )
import GHC.Core.TyCon
  ( TyCon(..), TyConBinder, TyConBndrVis(..)
#if MIN_VERSION_ghc(9,2,0)
  , isForgetfulSynTyCon
#endif
  , isFamFreeTyCon, isTypeSynonymTyCon
  , isTypeFamilyTyCon
  , tyConBinders, tyConResKind
  , tyConArity
  )
import GHC.Core.Type
  ( TyVar
  , tcView , mkTyConApp, mkScaled, coreView , tymult, tyVarKind
  )
#if MIN_VERSION_ghc(9,2,0)
import GHC.Data.Maybe
  ( firstJustsM )
#endif
import GHC.Tc.Plugin
  ( newWanted, newDerived )
import GHC.Tc.Solver.Monad
  ( TcS
  , zonkCo, zonkTcType
  , isFilledMetaTyVar_maybe
  , getInertEqs
  , checkReductionDepth
  , matchFam
  , runTcPluginTcS, runTcSWithEvBinds
  , traceTcS
#if MIN_VERSION_ghc(9,2,0)
  , lookupFamAppCache, lookupFamAppInert, extendFamAppCache
  , pattern EqualCtList
#else
  , lookupFlatCache, extendFlatCache
#endif
  )
import GHC.Tc.Types
  ( TcPluginM, TcPluginResult(..)
  , unsafeTcPluginTcM, getEvBindsTcPluginM
  )
import GHC.Tc.Types.Constraint
  ( Ct(..)
  , CtLoc, CtFlavour(..), CtFlavourRole, ShadowInfo(..)
#if MIN_VERSION_ghc(9,2,0)
  , CanEqLHS(..)
#endif
  , ctLoc, ctFlavour, ctEvidence, ctEqRel, ctEvPred
  , ctEvExpr, ctEvCoercion, ctEvFlavour
  , bumpCtLocDepth, eqCanRewriteFR, mkNonCanonical
  )
import GHC.Tc.Types.Evidence
  ( EvTerm(..), Role(..)
  , evCast, mkTcTransCo , mkTcTyConAppCo
  )
import GHC.Tc.Utils.TcType
  ( TcTyCoVarSet
#if MIN_VERSION_ghc(9,2,0)
  , tcSplitForAllTyVarBinders
#else
  , tcSplitForAllVarBndrs
#endif
  , tcSplitTyConApp_maybe
  , tcTypeKind
  , tyCoVarsOfType
  )
import GHC.Types.Unique.FM
  ( UniqFM, lookupUFM, isNullUFM )
import GHC.Types.Var
  ( TcTyVar, VarBndr(..)
#if !MIN_VERSION_ghc(9,2,0)
  , TyVarBinder
#endif
  , updateTyVarKindM
  )
import GHC.Types.Var.Env
  ( lookupDVarEnv )
import GHC.Types.Var.Set
  ( emptyVarSet )
import GHC.Utils.Misc
  ( dropList )
import GHC.Utils.Monad
  ( zipWith3M )
import GHC.Utils.Outputable
  ( Outputable(..), SDoc, empty )

-- ghc-tcplugin-api
import GHC.TcPlugin.API.Internal.Shim.Reduction

--------------------------------------------------------------------------------

data RewriteEnv
  = FE { fe_loc     :: !CtLoc
       , fe_flavour :: !CtFlavour
       , fe_eq_rel  :: !EqRel
       }

type TcPluginSolveResult = TcPluginResult

data TcPluginRewriteResult
  =
  -- | The plugin does not rewrite the type family application.
    TcPluginNoRewrite

  -- | The plugin rewrites the type family application
  -- providing a rewriting together with evidence.
  --
  -- The plugin can also emit additional wanted constraints.
  | TcPluginRewriteTo
    { tcPluginReduction :: !Reduction
    , tcRewriterWanteds :: [Ct]
    }

type Rewriter = RewriteEnv -> [Ct] -> [Type] -> TcPluginM TcPluginRewriteResult

type Rewriters = UniqFM TyCon Rewriter

shimRewriter :: [Ct] -> [Ct] -> [Ct]
             -> Rewriters
             -> ( [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult )
             -> TcPluginM TcPluginSolveResult
shimRewriter givens deriveds wanteds rws solver
  | isNullUFM rws
  = solver givens deriveds wanteds
  | otherwise
  = do
    res <- solver givens deriveds wanteds
    case res of
      contra@( TcPluginContradiction {} ) ->
        pure contra
      TcPluginOk solved new -> do
        ( rewrittenDeriveds, solvedDeriveds, newCts1 ) <- traverseCts ( reduceCt rws givens ) deriveds
        ( rewrittenWanteds , solvedWanteds , newCts2 ) <- traverseCts ( reduceCt rws givens ) wanteds
        pure $
            TcPluginOk
              ( solved ++ solvedDeriveds ++ solvedWanteds )
              ( new ++ newCts1 ++ rewrittenDeriveds ++ newCts2 ++ rewrittenWanteds )

reduceCt :: Rewriters
         -> [Ct]
         -> Ct
         -> TcPluginM ( Maybe ( Ct, (EvTerm, Ct) ), [Ct] )
reduceCt rws givens ct = do
  let
    predTy :: Type
    predTy = ctEvPred ( ctEvidence ct )
    rwEnv :: RewriteEnv
    rwEnv = FE ( ctLoc ct ) ( ctFlavour ct ) ( ctEqRel ct )
    shimRewriteEnv :: ShimRewriteEnv
    shimRewriteEnv = ShimRewriteEnv rws rwEnv givens
  ( res, newCts ) <- runRewritePluginM shimRewriteEnv ( rewrite_one predTy )
  case res of
    Nothing -> pure ( Nothing, newCts )
    Just ( Reduction co predTy' ) -> do
      ctEv' <- case ctFlavour ct of
        Given     -> error "ghc-tcplugin-api: unexpected Given in reduceCt"
        Wanted {} -> newWanted  ( ctLoc ct ) predTy'
        Derived   -> newDerived ( ctLoc ct ) predTy'
      pure ( Just
              ( mkNonCanonical ctEv'
              , ( evCast ( ctEvExpr ctEv' ) ( mkSymCo co )
                , ct
                )
              )
           , newCts
           )

traverseCts :: Monad m
            => ( a -> m ( Maybe (b, c), [d] ) )
            -> [a]
            -> m ( [b], [c], [d] )
traverseCts _ [] = pure ( [], [], [] )
traverseCts f (a : as) = do
  ( mb_bc, ds ) <- f a
  ( bs, cs, dss ) <- traverseCts f as
  case mb_bc of
    Nothing    -> pure ( bs, cs, ds ++ dss )
    Just (b,c) -> pure ( b : bs, c : cs, ds ++ dss )

--------------------------------------------------------------------------------
-- The following is (mostly) copied from GHC 9.4's GHC.Tc.Solver.Rewrite module.

rewrite_one :: Type -> RewriteM Reduction
rewrite_one ty
  | Just ty' <- rewriterView ty  -- See Note [Rewriting synonyms]
  = rewrite_one ty'

rewrite_one xi@(LitTy {})
  = do { role <- getRole
       ; return $ mkReflRedn role xi }

rewrite_one (TyVarTy tv)
  = rewriteTyVar tv

rewrite_one (AppTy ty1 ty2)
  = rewrite_app_tys ty1 [ty2]

rewrite_one (TyConApp tc tys)
  | isTypeFamilyTyCon tc
  = rewrite_fam_app tc tys

  | otherwise
  = rewrite_ty_con_app tc tys

rewrite_one (FunTy { ft_af = vis, ft_mult = mult, ft_arg = ty1, ft_res = ty2 })
  = do { arg_redn <- rewrite_one ty1
       ; res_redn <- rewrite_one ty2
       ; w_redn <- setEqRel NomEq $ rewrite_one mult
       ; role <- getRole
       ; return $ mkFunRedn role vis w_redn arg_redn res_redn }

rewrite_one ty@(ForAllTy {})
  = do { let (bndrs, rho) = tcSplitForAllTyVarBinders ty
       ; redn <- rewrite_one rho
       ; return $ mkHomoForAllRedn bndrs redn }

rewrite_one (CastTy ty g)
  = do { redn <- rewrite_one ty
       ; g'   <- rewrite_co g
       ; role <- getRole
       ; return $ mkCastRedn1 role ty g' redn }

rewrite_one (CoercionTy co)
  = do { co' <- rewrite_co co
       ; role <- getRole
       ; return $ mkReflCoRedn role co' }

rewrite_reduction :: Reduction -> RewriteM Reduction
rewrite_reduction (Reduction co xi) = do
  redn <- bumpDepth $ rewrite_one xi
  pure $ co `mkTransRedn` redn

rewrite_app_tys :: Type -> [Type] -> RewriteM Reduction
rewrite_app_tys (AppTy ty1 ty2) tys = rewrite_app_tys ty1 (ty2:tys)
rewrite_app_tys fun_ty arg_tys
  = do { redn <- rewrite_one fun_ty
       ; rewrite_app_ty_args redn arg_tys }

rewrite_app_ty_args :: Reduction -> [Type] -> RewriteM Reduction
rewrite_app_ty_args redn []
  = return redn
rewrite_app_ty_args fun_redn@(Reduction fun_co fun_xi) arg_tys
  = do { het_redn <- case tcSplitTyConApp_maybe fun_xi of
           Just (tc, xis) ->
             do { let tc_roles  = tyConRolesRepresentational tc
                      arg_roles = dropList xis tc_roles
                ; ArgsReductions (Reductions arg_cos arg_xis) kind_co
                    <- rewrite_vector (tcTypeKind fun_xi) arg_roles arg_tys

                ; eq_rel <- getEqRel
                ; let app_xi = mkTyConApp tc (xis ++ arg_xis)
                      app_co = case eq_rel of
                        NomEq  -> mkAppCos fun_co arg_cos
                        ReprEq -> mkAppCos fun_co (map mkNomReflCo arg_tys)
                                  `mkTcTransCo`
                                  mkTcTyConAppCo Representational tc
                                    (zipWith mkReflCo tc_roles xis ++ arg_cos)

                ; return $
                    mkHetReduction
                      (mkReduction app_co app_xi )
                      kind_co }
           Nothing ->
             do { ArgsReductions redns kind_co
                    <- rewrite_vector (tcTypeKind fun_xi) (repeat Nominal) arg_tys
                ; return $ mkHetReduction (mkAppRedns fun_redn redns) kind_co }

       ; role <- getRole
       ; return (homogeniseHetRedn role het_redn) }

{-# INLINE rewrite_args_tc #-}
rewrite_args_tc :: TyCon -> Maybe [Role] -> [Type] -> RewriteM ArgsReductions
rewrite_args_tc tc = rewrite_args all_bndrs any_named_bndrs inner_ki emptyVarSet
  -- NB: TyCon kinds are always closed
  where
  -- There are many bang patterns in here. It's been observed that they
  -- greatly improve performance of an optimized build.
  -- The T9872 test cases are good witnesses of this fact.

    (bndrs, named)
      = ty_con_binders_ty_binders' (tyConBinders tc)
    -- it's possible that the result kind has arrows (for, e.g., a type family)
    -- so we must split it
    (inner_bndrs, inner_ki, inner_named) = split_pi_tys' (tyConResKind tc)
    !all_bndrs                           = bndrs `chkAppend` inner_bndrs
    !any_named_bndrs                     = named || inner_named
    -- NB: Those bangs there drop allocations in T9872{a,c,d} by 8%.

rewrite_fam_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_fam_app tc tys = do
  let (tys1, tys_rest) = splitAt (tyConArity tc) tys
  redn <- rewrite_exact_fam_app tc tys1
  rewrite_app_ty_args redn tys_rest

rewrite_exact_fam_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_exact_fam_app tc tys = do
  checkStackDepth $ mkTyConApp tc tys
  rws <- getRewriters
  let
    mbRewriter :: Maybe Rewriter
    mbRewriter = lookupUFM rws tc
  result1 <- try_to_reduce tc tys mbRewriter
  case result1 of
    Just redn -> finish False redn
    _ -> do
      eq_rel <- getEqRel
      ArgsReductions (Reductions cos xis) kind_co <-
        if eq_rel == NomEq
        then rewrite_args_tc tc Nothing tys
        else setEqRel NomEq $
             rewrite_args_tc tc Nothing tys
      let
        role    = eqRelRole eq_rel
        args_co = mkTyConAppCo role tc cos
        homogenise :: Reduction -> Reduction
        homogenise redn
          = homogeniseHetRedn role
          $ mkHetReduction
              (args_co `mkTransRedn` redn)
              kind_co
        give_up :: Reduction
        give_up = homogenise $ mkReflRedn role (mkTyConApp tc xis)

      result2 <- liftTcS $ lookupFamAppInert tc xis
      flavour <- getFlavour
      case result2 of
        Just (co, xi, fr@(_, inert_eq_rel))
          | fr `eqCanRewriteFR` (flavour, eq_rel)
          , let
              redn :: Reduction
              redn = Reduction (mkSymCo co) xi -- inerts use a different orientation in GHC 9.0 and 9.2
          -> finish True (homogenise $ downgradeRedn role' inert_role redn)
          where
            inert_role      = eqRelRole inert_eq_rel
            role'           = eqRelRole eq_rel
        _ -> do
          result3 <- try_to_reduce tc xis mbRewriter
          case result3 of
            Just redn -> finish True (homogenise redn)
            _         -> return give_up
  where
    finish :: Bool -> Reduction -> RewriteM Reduction
    finish use_cache (Reduction co xi) = do
      Reduction fully_co fully <- bumpDepth $ rewrite_one xi
      let final_redn@(Reduction final_co final_xi) = Reduction (fully_co `mkTcTransCo` co) fully
      eq_rel <- getEqRel
      flavour <- getFlavour
      when (use_cache && eq_rel == NomEq && flavour /= Derived) $
        liftTcS $
          extendFamAppCache tc tys
            ( mkSymCo final_co, final_xi ) -- different orientation in GHC 9.0 and 9.2
#if !MIN_VERSION_ghc(9,2,0)
            flavour
#endif
      return final_redn
    {-# INLINE finish #-}

-- Returned coercion is output ~r input, where r is the role in the RewriteM monad
-- See Note [How to normalise a family application]
try_to_reduce :: TyCon -> [Type] -> Maybe Rewriter
              -> RewriteM (Maybe Reduction)
try_to_reduce tc tys mb_rewriter = do
  result <-
    firstJustsM
      [ runTcPluginRewriter mb_rewriter tys
      , liftTcS $ mkRed <$> lookupFamAppCache tc tys
      , liftTcS $ mkRed <$> matchFam tc tys ]
  forM result downgrade
    where
      mkRed :: Maybe (Coercion, Type) -> Maybe Reduction
      mkRed = fmap $ uncurry Reduction
      downgrade :: Reduction -> RewriteM Reduction
      downgrade redn@(Reduction co xi) = do
        eq_rel <- getEqRel
        case eq_rel of
          NomEq  -> return redn
          ReprEq -> return $ Reduction (mkSubCo co) xi

runTcPluginRewriter :: Maybe Rewriter
                    -> [Type]
                    -> RewriteM (Maybe Reduction)
runTcPluginRewriter mbRewriter tys =
  case mbRewriter of
    Nothing -> return Nothing
    Just rewriter -> do
      traceRewriteM "runTcPluginRewriter { " empty
      res <- runRewriter rewriter
      traceRewriteM "runTcPluginRewriter }" ( ppr res )
      pure res
  where
  runRewriter :: Rewriter -> RewriteM (Maybe Reduction)
  runRewriter rewriter = do
    rewriteResult <- RewriteM \ env s -> do
      res <- runTcPluginTcS ( rewriter ( rewriteEnv env ) ( rewriteGivens env ) tys )
      pure ( res, s )
    case rewriteResult of
      TcPluginRewriteTo
        { tcPluginReduction = redn
        , tcRewriterWanteds = wanteds
        } -> addRewriting ( Just redn ) wanteds
      TcPluginNoRewrite { }
          -> addRewriting Nothing []

rewrite_ty_con_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_ty_con_app tc tys
  = do { role <- getRole
       ; let m_roles | Nominal <- role = Nothing
                     | otherwise       = Just $ tyConRolesX role tc
       ; ArgsReductions redns kind_co <- rewrite_args_tc tc m_roles tys
       ; let tyconapp_redn
                = mkHetReduction
                    (mkTyConAppRedn role tc redns)
                    kind_co
       ; return $ homogeniseHetRedn role tyconapp_redn }

rewrite_co :: Coercion -> RewriteM Coercion
rewrite_co co = liftTcS $ zonkCo co

rewriterView :: Type -> Maybe Type
rewriterView ty@(TyConApp tc _)
  | ( isTypeSynonymTyCon tc && not (isFamFreeTyCon tc) )
#if MIN_VERSION_ghc(9,2,0)
  || isForgetfulSynTyCon tc
#endif
  = tcView ty
rewriterView _other = Nothing

rewriteTyVar :: TyVar -> RewriteM Reduction
rewriteTyVar tv = do
  mb_yes <- rewrite_tyvar1 tv
  case mb_yes of
    RTRFollowed redn -> rewrite_reduction redn
    RTRNotFollowed -> do
      tv' <- liftTcS $ updateTyVarKindM zonkTcType tv
      role <- getRole
      let ty' = mkTyVarTy tv'
      pure $ mkReflRedn role ty'

data RewriteTvResult
  = RTRNotFollowed
  | RTRFollowed !Reduction

rewrite_tyvar1 :: TcTyVar -> RewriteM RewriteTvResult
rewrite_tyvar1 tv = do
  mb_ty <- liftTcS $ isFilledMetaTyVar_maybe tv
  case mb_ty of
    Just ty -> do
      role <- getRole
      return $ RTRFollowed $ mkReflRedn role ty
    Nothing -> do
      fr <- getFlavourRole
      rewrite_tyvar2 tv fr

rewrite_tyvar2 :: TcTyVar -> CtFlavourRole -> RewriteM RewriteTvResult
rewrite_tyvar2 tv fr@(_, eq_rel) = do
  ieqs <- liftTcS $ getInertEqs
  case lookupDVarEnv ieqs tv of
#if MIN_VERSION_ghc(9,2,0)
    Just (EqualCtList (ct :| _))
      | CEqCan { cc_ev = ctev, cc_lhs = TyVarLHS {}
               , cc_rhs = rhs_ty, cc_eq_rel = ct_eq_rel } <- ct
#else
    Just (ct : _)
      | CTyEqCan { cc_ev = ctev
                 , cc_rhs = rhs_ty, cc_eq_rel = ct_eq_rel } <- ct
#endif
      , let ct_fr = (ctEvFlavour ctev, ct_eq_rel)
      , ct_fr `eqCanRewriteFR` fr
      -> do
          let rewriting_co1 = ctEvCoercion ctev
              rewriting_co  = case (ct_eq_rel, eq_rel) of
                (ReprEq, _rel)  -> rewriting_co1
                (NomEq, NomEq)  -> rewriting_co1
                (NomEq, ReprEq) -> mkSubCo rewriting_co1
          return $ RTRFollowed $ mkReduction rewriting_co rhs_ty 
    _other -> return RTRNotFollowed

rewrite_vector :: Kind
               -> [Role]
               -> [Type]
               -> RewriteM ArgsReductions
rewrite_vector ki roles tys
  = do { eq_rel <- getEqRel
       ; let mb_roles = case eq_rel of { NomEq -> Nothing; ReprEq -> Just roles }
       ; rewrite_args bndrs any_named_bndrs inner_ki fvs mb_roles tys
       }
  where
    (bndrs, inner_ki, any_named_bndrs) = split_pi_tys' ki
    fvs                                = tyCoVarsOfType ki
{-# INLINE rewrite_vector #-}

split_pi_tys' :: Type -> ([TyCoBinder], Type, Bool)
split_pi_tys' ty = split ty ty
  where
  split _ (ForAllTy b res) =
    let !(bs, ty', _) = split res res
    in  (Named b : bs, ty', True)
  split _ (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res }) =
    let !(bs, ty', named) = split res res
    in  (Anon af (mkScaled w arg) : bs, ty', named)
  split orig_ty ty' | Just ty'' <- coreView ty' = split orig_ty ty''
  split orig_ty _ = ([], orig_ty, False)
{-# INLINE split_pi_tys' #-}

ty_con_binders_ty_binders' :: [TyConBinder] -> ([TyCoBinder], Bool)
ty_con_binders_ty_binders' = foldr go ([], False)
  where
    go (Bndr tv (NamedTCB vis)) (bndrs, _)
      = (Named (Bndr tv vis) : bndrs, True)
    go (Bndr tv (AnonTCB af))   (bndrs, n)
      = (Anon af (tymult (tyVarKind tv)) : bndrs, n)
    {-# INLINE go #-}
{-# INLINE ty_con_binders_ty_binders' #-}

{-# INLINE rewrite_args #-}
rewrite_args :: [TyCoBinder] -> Bool
             -> Kind -> TcTyCoVarSet
             -> Maybe [Role] -> [Type]
             -> RewriteM ArgsReductions
rewrite_args orig_binders
             any_named_bndrs
             orig_inner_ki
             orig_fvs
             orig_m_roles
             orig_tys
  = case (orig_m_roles, any_named_bndrs) of
      (Nothing, False) -> rewrite_args_fast orig_tys
      _ -> rewrite_args_slow orig_binders orig_inner_ki orig_fvs orig_roles orig_tys
        where orig_roles = fromMaybe (repeat Nominal) orig_m_roles

{-# INLINE rewrite_args_fast #-}
rewrite_args_fast :: [Type] -> RewriteM ArgsReductions
rewrite_args_fast orig_tys
  = fmap finish (iterate orig_tys)
  where

    iterate :: [Type] -> RewriteM Reductions
    iterate (ty : tys) = do
      Reduction  co  xi  <- rewrite_one ty
      Reductions cos xis <- iterate tys
      pure $ Reductions (co : cos) (xi : xis)
    iterate [] = pure $ Reductions [] []

    {-# INLINE finish #-}
    finish :: Reductions -> ArgsReductions
    finish redns = ArgsReductions redns MRefl

{-# INLINE rewrite_args_slow #-}
rewrite_args_slow :: [TyCoBinder] -> Kind -> TcTyCoVarSet
                  -> [Role] -> [Type]
                  -> RewriteM ArgsReductions
rewrite_args_slow binders inner_ki fvs roles tys
  = do { rewritten_args <- zipWith3M fl (map isNamedBinder binders ++ repeat True)
                                        roles tys
       ; return $ simplifyArgsWorker binders inner_ki fvs roles rewritten_args }
  where
    {-# INLINE fl #-}
    fl :: Bool   -- must we ensure to produce a real coercion here?
                 -- see comment at top of function
       -> Role -> Type -> RewriteM Reduction
    fl True  r ty = noBogusCoercions $ fl1 r ty
    fl False r ty =                    fl1 r ty

    {-# INLINE fl1 #-}
    fl1 :: Role -> Type -> RewriteM Reduction
    fl1 Nominal ty
      = setEqRel NomEq $
        rewrite_one ty

    fl1 Representational ty
      = setEqRel ReprEq $
        rewrite_one ty

    fl1 Phantom ty
      = do { ty' <- liftTcS $ zonkTcType ty
           ; return $ mkReflRedn Phantom ty' }

noBogusCoercions :: RewriteM a -> RewriteM a
noBogusCoercions thing_inside
  = RewriteM \ env s ->
    let !renv  = rewriteEnv env
        !renv' = case fe_flavour renv of
          Derived -> renv { fe_flavour = Wanted WDeriv }
          _       -> renv
        !env' = env { rewriteEnv = renv' }
    in
    runRewriteM thing_inside env' s

chkAppend :: [a] -> [a] -> [a]
chkAppend xs ys
  | null ys   = xs
  | otherwise = xs ++ ys

--------------------------------------------------------------------------------

data ReduceQ = NoReduction | DidReduce
instance Semigroup ReduceQ where
  NoReduction <> NoReduction = NoReduction
  _ <> _ = DidReduce
instance Monoid ReduceQ where
  mempty = NoReduction

data RewriteState =
  RewriteState
   { rewrittenCts      :: ![ Ct ]
   , reductionOccurred :: !ReduceQ
   }

data ShimRewriteEnv
  = ShimRewriteEnv
  { rewriters     :: !Rewriters
  , rewriteEnv    :: !RewriteEnv
  , rewriteGivens :: ![ Ct ]
  }

newtype RewriteM a
  = RewriteM
  { runRewriteM
    :: ShimRewriteEnv
    -> RewriteState
    -> TcS ( a, RewriteState )
  }
  deriving ( Functor, Applicative, Monad )
    via ( ReaderT ShimRewriteEnv
          ( StateT RewriteState TcS )
        )

runRewritePluginM :: ShimRewriteEnv
                  -> RewriteM a
                  -> TcPluginM ( Maybe a, [Ct] )
runRewritePluginM env ( RewriteM { runRewriteM = run } ) = do

  evBindsVar <- getEvBindsTcPluginM
  ( a, RewriteState { rewrittenCts, reductionOccurred } )
    <- unsafeTcPluginTcM
     $ runTcSWithEvBinds evBindsVar
     $ run env ( RewriteState [] NoReduction )
  let
    mb_a = case reductionOccurred of
      NoReduction -> Nothing
      DidReduce   -> Just a
  pure ( mb_a, rewrittenCts )

addRewriting :: Maybe Reduction -> [Ct] -> RewriteM ( Maybe Reduction )
addRewriting mbRedn newCts = RewriteM \ _ ( RewriteState cts s ) ->
  let
    s' :: ReduceQ
    s'
      | Just _ <- mbRedn
      = DidReduce
      | otherwise
      = s
  in pure ( mbRedn , RewriteState ( cts <> newCts ) s' )

getRewriters :: RewriteM Rewriters
getRewriters = RewriteM \ env s -> pure ( rewriters env, s )

getRewriteEnvField :: (RewriteEnv -> a) -> RewriteM a
getRewriteEnvField accessor = RewriteM \ env s ->
  pure ( accessor ( rewriteEnv env ), s )

getEqRel :: RewriteM EqRel
getEqRel = getRewriteEnvField fe_eq_rel

getRole :: RewriteM Role
getRole = eqRelRole <$> getEqRel

getFlavour :: RewriteM CtFlavour
getFlavour = getRewriteEnvField fe_flavour

getFlavourRole :: RewriteM CtFlavourRole
getFlavourRole = do
  flavour <- getFlavour
  eq_rel <- getEqRel
  return (flavour, eq_rel)

setEqRel :: EqRel -> RewriteM a -> RewriteM a
setEqRel new_eq_rel thing_inside = RewriteM \ env s ->
  if new_eq_rel == fe_eq_rel ( rewriteEnv env )
  then runRewriteM thing_inside env s
  else runRewriteM thing_inside ( setEqRel' env ) s
    where
      setEqRel' :: ShimRewriteEnv -> ShimRewriteEnv
      setEqRel' env = env { rewriteEnv = ( rewriteEnv env ) { fe_eq_rel = new_eq_rel } }
{-# INLINE setEqRel #-}

liftTcS :: TcS a -> RewriteM a
liftTcS thing_inside = RewriteM \ _env s -> do
  a <- thing_inside
  pure ( a, s )

traceRewriteM :: String -> SDoc -> RewriteM ()
traceRewriteM herald doc = liftTcS $ traceTcS herald doc
{-# INLINE traceRewriteM #-}

getLoc :: RewriteM CtLoc
getLoc = getRewriteEnvField fe_loc

checkStackDepth :: Type -> RewriteM ()
checkStackDepth ty = do
  loc <- getLoc
  liftTcS $ checkReductionDepth loc ty

bumpDepth :: RewriteM a -> RewriteM a
bumpDepth (RewriteM thing_inside) = RewriteM \ env s -> do
  let !renv  = rewriteEnv env
      !renv' = renv { fe_loc = bumpCtLocDepth ( fe_loc renv ) }
      !env'  = env { rewriteEnv = renv' }
  thing_inside env' s

#if !MIN_VERSION_ghc(9,2,0)
--------------------------------------------------------------------------------
-- GHC 9.0 compatibility.

firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing where
  go :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
  go Nothing         action  = action
  go result@(Just _) _action = return result

lookupFamAppCache :: TyCon -> [Type] -> TcS (Maybe (Coercion, Type))
lookupFamAppCache fam_tc tys = do
  res <- lookupFlatCache fam_tc tys
  pure $ case res of
    Nothing -> Nothing
    Just  ( co, ty, _ ) -> Just ( co, ty )

extendFamAppCache :: TyCon -> [Type] -> (Coercion, Type) -> CtFlavour -> TcS ()
extendFamAppCache tc xi_args (co, ty) f = extendFlatCache tc xi_args (co, ty, f)

lookupFamAppInert :: TyCon -> [Type] -> TcS (Maybe (Coercion, Type, CtFlavourRole))
lookupFamAppInert tc tys = do
  res <- lookupFlatCache tc tys
  pure $ case res of
    Nothing -> Nothing
    Just ( co, ty, f ) -> Just ( co, ty, (f, NomEq) )

tcSplitForAllTyVarBinders :: Type -> ([TyVarBinder], Type)
tcSplitForAllTyVarBinders = tcSplitForAllVarBndrs

#endif
