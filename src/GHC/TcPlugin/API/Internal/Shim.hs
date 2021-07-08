{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

{-|
Module: GHC.TcPlugin.API.Internal.Shim

This module defines a compatibility shim which allows
the library to support a limited form of type-family rewriting
in typechecking plugins on GHC 9.0 and 9.2.
-}

module GHC.TcPlugin.API.Internal.Shim where

-- base
import Control.Monad
  ( forM, when )
#if !MIN_VERSION_ghc(9,2,0)
import Data.Foldable
  ( foldlM )
#endif
import Data.IORef
  ( IORef, readIORef, writeIORef )
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
  ( castCoercionKind1
  , mkReflCo, mkSymCo, mkFunCo, mkHomoForAllCos
  , mkTransCo, mkAppCos, mkNomReflCo, mkSubCo
  , mkTyConAppCo, tyConRolesX
  , tyConRolesRepresentational
  , simplifyArgsWorker
#if !MIN_VERSION_ghc(9,2,0)
  , coToMCo
#endif
  )
#if MIN_VERSION_ghc(9,2,0)
import GHC.Core.Map.Type
  ( LooseTypeMap )
#else
import GHC.Core.Map
  ( LooseTypeMap )
#endif
import GHC.Core.Predicate
  ( EqRel(..), eqRelRole )
import GHC.Core.TyCo.Rep
  ( Type(..), Kind, Coercion(..)
  , TyCoBinder(..)
  , MCoercion(..), MCoercionN
  , binderVars, mkForAllTys
  , isNamedBinder
  , mkTyVarTy
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
  , tcView
  , mkCoercionTy, mkCastTy, mkAppTys
  , mkTyConApp, mkScaled, coreView
  , tymult, tyVarKind
  )
#if MIN_VERSION_ghc(9,2,0)
import GHC.Data.Maybe
  ( firstJustsM )
#endif
import GHC.Data.TrieMap
  ( ListMap )
import GHC.Tc.Plugin
  ( tcPluginIO
  , newWanted, newDerived
  )
import GHC.Tc.Solver.Monad
  ( TcS
  , zonkCo, zonkTcType
  , isFilledMetaTyVar_maybe
  , getInertEqs
  , checkReductionDepth
  , matchFam, findFunEq, insertFunEq
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
  , Xi
#if MIN_VERSION_ghc(9,2,0)
  , CanEqLHS(..)
#endif
  , ctLoc, ctFlavour, ctEvidence, ctEqRel, ctEvPred
  , ctEvExpr, ctEvCoercion, ctEvFlavour
  , bumpCtLocDepth, eqCanRewriteFR, mkNonCanonical
  )
import GHC.Tc.Types.Evidence
  ( EvTerm(..), Role(..)
  , evCast
  , mkTcReflCo, mkTcTransCo, mkTcSymCo
  , mkTcTyConAppCo
  , tcDowngradeRole
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
#if !MIN_VERSION_ghc(9,2,0)
import GHC.Types.Unique
  ( Unique )
#endif
import GHC.Types.Unique.DFM
  ( UniqDFM )
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
  ( Outputable(ppr), SDoc
  , empty, text, ($$)
  )

--------------------------------------------------------------------------------

-- | A reduction to the provided type, with a coercion witnessing the equality.
data Reduction = Reduction !Type Coercion
instance Outputable Reduction where
  ppr (Reduction ty co) = text "Reduction" $$ ppr ty $$ ppr co

data RewriteEnv
  = FE { fe_loc     :: !CtLoc
       , fe_flavour :: !CtFlavour
       , fe_eq_rel  :: !EqRel
       }

mkReduction :: ( Coercion, Type ) -> Reduction
mkReduction ( co, ty ) = Reduction ty co

runReduction1 :: Reduction -> ( Type, Coercion )
runReduction1 ( Reduction ty co ) = ( ty, co )

runReduction2 :: Reduction -> ( Coercion, Type )
runReduction2 ( Reduction ty co ) = ( co, ty )

type TcPluginSolveResult = TcPluginResult

data TcPluginRewriteResult
  =
  -- | The plugin does not rewrite the type family application.
  --
  -- The plugin can also emit additional wanted constraints,
  -- including insoluble ones (e.g. a type error message).
    TcPluginNoRewrite { tcRewriterWanteds :: [Ct] }

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
             -> IORef RewrittenTyFamApps
             -> Rewriters
             -> ( [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult )
             -> TcPluginM TcPluginSolveResult
shimRewriter givens deriveds wanteds cacheRef rws solver
  | isNullUFM rws
  = solver givens deriveds wanteds
  | otherwise
  = do
    ( solvedDeriveds, newCts1, deriveds') <- traverseCts ( reduceCt cacheRef rws givens ) deriveds
    ( solvedWanteds , newCts2, wanteds' ) <- traverseCts ( reduceCt cacheRef rws givens ) wanteds
    res <- solver givens deriveds' wanteds'
    case res of
      contra@( TcPluginContradiction {} )
        -> pure contra
      TcPluginOk solved new
        -> pure $
              TcPluginOk
                ( solvedDeriveds ++ solvedWanteds ++ solved )
                ( newCts1 ++ newCts2 ++ new )

reduceCt :: IORef RewrittenTyFamApps
         -> Rewriters
         -> [Ct]
         -> Ct
         -> TcPluginM ( Maybe (EvTerm, Ct), [Ct], Ct )
reduceCt cacheRef rws givens ct = do
  let
    predTy :: Type
    predTy = ctEvPred ( ctEvidence ct )
    rwEnv :: RewriteEnv
    rwEnv = FE ( ctLoc ct ) ( ctFlavour ct ) ( ctEqRel ct )
    shimRewriteEnv :: ShimRewriteEnv
    shimRewriteEnv = ShimRewriteEnv rws rwEnv ct givens cacheRef
  ( res, newCts ) <- runRewritePluginM shimRewriteEnv ( rewrite_one predTy )
  case res of
    Nothing -> pure ( Nothing, newCts, ct )
    Just ( Reduction predTy' co ) -> do
      ctEv' <- case ctFlavour ct of
        Given     -> error "ghc-tcplugin-api: unexpected Given in reduceCt"
        Wanted {} -> newWanted  ( ctLoc ct ) predTy'
        Derived   -> newDerived ( ctLoc ct ) predTy'
      pure ( Just ( evCast ( ctEvExpr ctEv' ) co, ct ), newCts, mkNonCanonical ctEv' )

traverseCts :: Monad m
            => ( a -> m ( Maybe b, [c], d ) )
            -> [a]
            -> m ( [b], [c], [d] )
traverseCts _ [] = pure ( [], [], [] )
traverseCts f (a : as) = do
  ( mb_b, cs, d ) <- f a
  ( bs, css, ds ) <- traverseCts f as
  pure ( maybe bs ( : bs ) mb_b, cs ++ css, d : ds )

--------------------------------------------------------------------------------
-- The following is (mostly) copied from GHC 9.4's GHC.Tc.Solver.Rewrite module.

rewrite_one :: Type -> RewriteM Reduction
rewrite_one = \case
  ( rewriterView -> Just ty' )
    -> rewrite_one ty'
  ty@( LitTy {} )
    -> do
      role <- getRole
      pure $ Reduction ty ( mkReflCo role ty )
  TyVarTy tv
    -> rewriteTyVar tv
  AppTy ty1 ty2
    -> rewrite_app_tys ty1 [ty2]
  TyConApp tc tys
    | isTypeFamilyTyCon tc
    -> rewrite_fam_app tc tys
    | otherwise
    -> rewrite_ty_con_app tc tys
  ty@( FunTy { ft_mult = mult, ft_arg = ty1, ft_res = ty2 } )
    -> do
      Reduction xi1 co1 <- rewrite_one ty1
      Reduction xi2 co2 <- rewrite_one ty2
      Reduction xi3 co3 <- setEqRel NomEq $ rewrite_one mult
      role <- getRole
      return $
       Reduction
         ( ty { ft_mult = xi3, ft_arg = xi1, ft_res = xi2 } )
         ( mkFunCo role co3 co1 co2 )
  ty@( ForAllTy {} )
    -> do
      let
        (bndrs, rho) = tcSplitForAllTyVarBinders ty
        tvs          = binderVars bndrs
      Reduction rho' co <- rewrite_one rho
      pure $ Reduction
        ( mkForAllTys bndrs rho' )
        ( mkHomoForAllCos tvs co )
  CastTy ty g
    -> do 
      Reduction xi co <- rewrite_one ty
      (g', _) <- rewrite_co g
      role <- getRole
      pure $ Reduction
        ( mkCastTy xi g' )
        ( castCoercionKind1 co role xi ty g' )
  CoercionTy co
    -> do
      ( co1, co2 ) <- rewrite_co co
      pure $ Reduction ( mkCoercionTy co1 ) co2

rewrite_app_tys :: Type -> [Type] -> RewriteM Reduction
rewrite_app_tys ( AppTy ty1 ty2 ) tys =
  rewrite_app_tys ty1 ( ty2 : tys )
rewrite_app_tys fun_ty arg_tys = do
  Reduction fun_xi fun_co <- rewrite_one fun_ty
  rewrite_app_ty_args fun_xi fun_co arg_tys

rewrite_app_ty_args :: Xi -> Coercion -> [Type] -> RewriteM Reduction
rewrite_app_ty_args fun_xi fun_co [] = pure $ Reduction fun_xi fun_co
rewrite_app_ty_args fun_xi fun_co arg_tys = do
  (xi, co, kind_co) <- case tcSplitTyConApp_maybe fun_xi of
    Just (tc, xis) -> do
      let tc_roles  = tyConRolesRepresentational tc
          arg_roles = dropList xis tc_roles
      (arg_xis, arg_cos, kind_co)
        <- rewrite_vector (tcTypeKind fun_xi) arg_roles arg_tys
      eq_rel <- getEqRel
      let app_xi = mkTyConApp tc (xis ++ arg_xis)
          app_co = case eq_rel of
            NomEq  -> mkAppCos fun_co arg_cos
            ReprEq -> mkTcTyConAppCo Representational tc
                        (zipWith mkReflCo tc_roles xis ++ arg_cos)
                      `mkTcTransCo`
                      mkAppCos fun_co (map mkNomReflCo arg_tys)
      return (app_xi, app_co, kind_co)
    Nothing -> do
      (arg_xis, arg_cos, kind_co)
        <- rewrite_vector (tcTypeKind fun_xi) (repeat Nominal) arg_tys
      let arg_xi = mkAppTys fun_xi arg_xis
          arg_co = mkAppCos fun_co arg_cos
      return (arg_xi, arg_co, kind_co)
  role <- getRole
  return (homogenise_result xi co role kind_co)

{-# INLINE rewrite_args_tc #-}
rewrite_args_tc :: TyCon -> Maybe [Role] -> [Type] -> RewriteM ( [Xi], [Coercion] , MCoercionN)
rewrite_args_tc tc = rewrite_args all_bndrs any_named_bndrs inner_ki emptyVarSet
  where
    (bndrs, named)
      = ty_con_binders_ty_binders' (tyConBinders tc)
    (inner_bndrs, inner_ki, inner_named) = split_pi_tys' (tyConResKind tc)
    !all_bndrs                           = bndrs `chkAppend` inner_bndrs
    !any_named_bndrs                     = named || inner_named

rewrite_fam_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_fam_app tc tys = do
  let (tys1, tys_rest) = splitAt (tyConArity tc) tys
  Reduction xi1 co1 <- rewrite_exact_fam_app tc tys1
  rewrite_app_ty_args xi1 co1 tys_rest

rewrite_exact_fam_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_exact_fam_app tc tys = do
  checkStackDepth (mkTyConApp tc tys)
  rws <- getRewriters
  let
    mbRewriter :: Maybe Rewriter
    mbRewriter = lookupUFM rws tc
  result1 <- try_to_reduce tc tys mbRewriter
  case result1 of
    Just redn -> finish False redn
    _ -> do
      eq_rel <- getEqRel
      (xis, coercions, kind_co) <-
        if eq_rel == NomEq
        then rewrite_args_tc tc Nothing tys
        else setEqRel NomEq $
             rewrite_args_tc tc Nothing tys
      let role    = eqRelRole eq_rel
          args_co = mkTyConAppCo role tc coercions
          homogenise :: Reduction -> Reduction
          homogenise (Reduction xi co) =
            homogenise_result xi (co `mkTcTransCo` args_co) role kind_co
          giveUp :: Reduction
          giveUp = homogenise $ Reduction reduced (mkTcReflCo role reduced)
             where reduced = mkTyConApp tc xis
      result2 <- liftTcS $ lookupFamAppInert tc xis
      flavour <- getFlavour
      case result2 of
        Just (co, xi, fr@(_, inert_eq_rel))
          | fr `eqCanRewriteFR` (flavour, eq_rel)
          -> finish True (homogenise $ Reduction xi downgraded_co)
          where
            inert_role    = eqRelRole inert_eq_rel
            role'         = eqRelRole eq_rel
            downgraded_co = tcDowngradeRole role' inert_role (mkTcSymCo co)
        _ -> do
          result3 <- try_to_reduce tc xis mbRewriter
          case result3 of
            Just redn -> finish True (homogenise redn)
            _         -> return giveUp
  where
    finish :: Bool -> Reduction -> RewriteM Reduction
    finish use_cache (Reduction xi co) = do
      Reduction fully fully_co <- bumpDepth $ rewrite_one xi
      let final_redn = Reduction fully (fully_co `mkTcTransCo` co)
      eq_rel <- getEqRel
      flavour <- getFlavour
      when (use_cache && eq_rel == NomEq && flavour /= Derived) $
        liftTcS $
          extendFamAppCache tc tys
            ( runReduction2 final_redn )
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
      [ runTcPluginRewriter mb_rewriter tc tys
      , liftTcS $ mkRed <$> lookupFamAppCache tc tys
      , liftTcS $ mkRed <$> matchFam tc tys ]
  forM result downgrade
    where
      mkRed :: Maybe (Coercion, Type) -> Maybe Reduction
      mkRed = fmap $ uncurry ( flip Reduction )
      downgrade :: Reduction -> RewriteM Reduction
      downgrade redn@(Reduction xi co) = do
        eq_rel <- getEqRel
        case eq_rel of
          NomEq  -> return redn
          ReprEq -> return $ Reduction xi (mkSubCo co)

runTcPluginRewriter :: Maybe Rewriter
                    -> TyCon -> [Type]
                    -> RewriteM (Maybe Reduction)
runTcPluginRewriter mbRewriter tc tys =
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
        } ->
        addRewriting tc tys ( Just redn ) wanteds
      TcPluginNoRewrite { tcRewriterWanteds = wanteds } -> do
        addRewriting tc tys Nothing wanteds

rewrite_ty_con_app :: TyCon -> [Type] -> RewriteM Reduction
rewrite_ty_con_app tc tys = do
  role <- getRole
  let m_roles | Nominal <- role = Nothing
              | otherwise       = Just $ tyConRolesX role tc
  (xis, coercions, kind_co) <- rewrite_args_tc tc m_roles tys
  let tyconapp_xi = mkTyConApp tc xis
      tyconapp_co = mkTyConAppCo role tc coercions
  return (homogenise_result tyconapp_xi tyconapp_co role kind_co)

rewrite_co :: Coercion -> RewriteM ( Coercion, Coercion )
rewrite_co co = do
  zonked_co <- liftTcS $ zonkCo co
  env_role <- getRole
  let co' = mkTcReflCo env_role ( mkCoercionTy zonked_co )
  pure ( zonked_co, co' )

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
    RTRFollowed ty1 co1 -> do
      Reduction ty2 co2 <- rewrite_one ty1
      pure $ Reduction ty2 (co2 `mkTransCo` co1)
    RTRNotFollowed -> do
      tv' <- liftTcS $ updateTyVarKindM zonkTcType tv
      role <- getRole
      let ty' = mkTyVarTy tv'
      return $ Reduction ty' (mkTcReflCo role ty')

data RewriteTvResult
  = RTRNotFollowed
  | RTRFollowed Type Coercion

rewrite_tyvar1 :: TcTyVar -> RewriteM RewriteTvResult
rewrite_tyvar1 tv = do
  mb_ty <- liftTcS $ isFilledMetaTyVar_maybe tv
  case mb_ty of
    Just ty -> do
      role <- getRole
      return (RTRFollowed ty (mkReflCo role ty))
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
        let rewrite_co1 = mkSymCo (ctEvCoercion ctev)
            rewrite_co2 = case (ct_eq_rel, eq_rel) of
              (ReprEq, _rel)  -> rewrite_co1
              (NomEq, NomEq)  -> rewrite_co1
              (NomEq, ReprEq) -> mkSubCo rewrite_co1
        return (RTRFollowed rhs_ty rewrite_co2)
    _other -> return RTRNotFollowed

rewrite_vector :: Kind 
               -> [Role]
               -> [Type]
               -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_vector ki roles tys = do
  eq_rel <- getEqRel
  case eq_rel of
    NomEq ->
      rewrite_args bndrs
        any_named_bndrs
        inner_ki
        fvs
        Nothing
        tys
    ReprEq ->
      rewrite_args bndrs
        any_named_bndrs
        inner_ki
        fvs
        (Just roles)
        tys
  where
    (bndrs, inner_ki, any_named_bndrs) = split_pi_tys' ki
    fvs                                = tyCoVarsOfType ki
{-# INLINE rewrite_vector #-}

homogenise_result :: Xi
                  -> Coercion
                  -> Role
                  -> MCoercionN
                  -> Reduction
homogenise_result xi co _ MRefl = Reduction xi co
homogenise_result xi co r mco@(MCo kind_co)
  = Reduction (xi `mkCastTy` kind_co) ((mkSymCo $ GRefl r xi mco) `mkTransCo` co)
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

rewrite_args :: [TyCoBinder] -> Bool
             -> Kind -> TcTyCoVarSet
             -> Maybe [Role] -> [Type]
             -> RewriteM ([Xi], [Coercion], MCoercionN)
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
rewrite_args_fast :: [Type]
                  -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_args_fast orig_tys
  = fmap finish (iterateRewrite orig_tys)
  where

    iterateRewrite :: [Type] -> RewriteM ([Xi], [Coercion])
    iterateRewrite (ty:tys) = do
      Reduction xi co <- rewrite_one ty
      (xis, coercions) <- iterateRewrite tys
      pure (xi : xis, co : coercions)
    iterateRewrite [] = pure ([], [])

    {-# INLINE finish #-}
    finish :: ([Xi], [Coercion]) -> ([Xi], [Coercion], MCoercionN)
    finish (xis, coercions) = (xis, coercions, MRefl)

{-# INLINE rewrite_args_slow #-}
rewrite_args_slow :: [TyCoBinder] -> Kind -> TcTyCoVarSet
                  -> [Role] -> [Type]
                  -> RewriteM ([Xi], [Coercion], MCoercionN)
rewrite_args_slow binders inner_ki fvs roles tys = do
  rewritten_args <-
    zipWith3M fl (map isNamedBinder binders ++ repeat True)
      roles tys
  pure
#if !MIN_VERSION_ghc(9,2,0)
    $ ( \ ( xs, cs, c ) -> ( xs, cs, coToMCo c ) )
#endif
    $ simplifyArgsWorker binders inner_ki fvs roles rewritten_args
  where
    {-# INLINE fl #-}
    fl :: Bool -> Role -> Type -> RewriteM ( Type, Coercion )
    fl True  r ty = noBogusCoercions $ runReduction1 <$> fl1 r ty
    fl False r ty =                    runReduction1 <$> fl1 r ty

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
           ; pure $ Reduction ty' ( mkReflCo Phantom ty' ) }

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

type RewrittenTyFamApps =
  UniqDFM
#if MIN_VERSION_ghc(9,2,0)
    TyCon
#else
    Unique
#endif
    ( ListMap LooseTypeMap ( Maybe Reduction, [Ct] ) )

data ShimRewriteEnv
  = ShimRewriteEnv
  { rewriters     :: !Rewriters
  , rewriteEnv    :: !RewriteEnv
  , rewriteCt     :: !Ct
  , rewriteGivens :: ![ Ct ]
  , rewriteCache  :: !( IORef RewrittenTyFamApps )
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
  ( a, RewriteState newCts didReduce )
    <- unsafeTcPluginTcM
     $ runTcSWithEvBinds evBindsVar
     $ run env ( RewriteState [] NoReduction )
  let
    mb_a = case didReduce of
      NoReduction -> Nothing
      DidReduce   -> Just a
  pure ( mb_a, newCts )

setDidReduce :: RewriteM ()
setDidReduce = RewriteM \ _env ( RewriteState cts _ ) ->
  pure ( (), RewriteState cts DidReduce )

addRewriting :: TyCon -> [Type] -> Maybe Reduction -> [Ct] -> RewriteM ( Maybe Reduction )
addRewriting tc tys mbRedn newCts = RewriteM \ env ( RewriteState cts s ) -> do
  rewritings <- liftIOTcS $ readIORef ( rewriteCache env )
  let
    s' :: ReduceQ
    s'
      | Just _ <- mbRedn
      = DidReduce
      | otherwise
      = s
    newRewritings :: RewrittenTyFamApps
    newRewritings = insertFunEq rewritings tc tys ( mbRedn, newCts )
    mbEmittedWork :: Maybe ( Maybe Reduction, [Ct] )
    mbEmittedWork = findFunEq rewritings tc tys
  case mbEmittedWork of
    -- We've already rewritten this.
    -- Avoid emitting constraints for it again,
    -- to avoid sending the constraint solver in a loop.
    -- TODO: this is quite fragile.
    Just _  -> pure ( mbRedn, RewriteState cts s' )
    Nothing -> do
      liftIOTcS $ writeIORef ( rewriteCache env ) newRewritings
      pure ( mbRedn , RewriteState ( cts <> newCts ) s' )

-- Silly workaround because wrapTcS is not exported in GHC 9.0
liftIOTcS :: IO a -> TcS a
liftIOTcS = runTcPluginTcS . tcPluginIO

getRewriters :: RewriteM Rewriters
getRewriters = RewriteM \ env s -> pure ( rewriters env, s )

getGivens :: RewriteM [Ct]
getGivens = RewriteM \ env s -> pure ( rewriteGivens env, s )

getRewriteCache :: RewriteM ( IORef RewrittenTyFamApps )
getRewriteCache = RewriteM \ env s -> pure ( rewriteCache env, s )

getRewriteCt :: RewriteM Ct
getRewriteCt = RewriteM \ env s -> pure ( rewriteCt env, s )

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
