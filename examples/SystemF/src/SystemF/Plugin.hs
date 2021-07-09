{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module SystemF.Plugin ( plugin ) where

-- base
import Data.Maybe
  ( isJust )

-- ghc
import qualified GHC.Plugins as GHC
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Utils.Outputable
  ( ($$) )

-- ghc-tcplugin-api
import GHC.TcPlugin.API

--------------------------------------------------------------------------------
-- Plugin definition and setup/finalisation.

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.tcPlugin        = \ _args -> Just $ mkTcPlugin tcPlugin
    , GHC.pluginRecompile = GHC.purePlugin
    }

tcPlugin :: TcPlugin
tcPlugin =
  TcPlugin
    { tcPluginInit    = pluginInit
    , tcPluginSolve   = pluginSolve
    , tcPluginRewrite = pluginRewrite
    , tcPluginStop    = pluginStop
    }

data PluginDefs =
  PluginDefs
    { applySubTyCon :: !TyCon
    , idTyCon       :: !TyCon
    , bindTyCon     :: !TyCon
    , underTyCon    :: !TyCon
    , extendTyCon   :: !TyCon
    , composeTyCon  :: !TyCon
    }

findTypesModule :: MonadTcPlugin m => m Module
findTypesModule = do
  findResult <- findImportedModule ( mkModuleName "SystemF.Type" ) Nothing
  case findResult of
    Found _ res     -> pure res
    FoundMultiple _ -> error $ "SystemF.Plugin: found multiple modules named SystemF.Type in the current package."
    _               -> error $ "SystemF.Plugin: could not find any module named SystemF.Type in the current package."

pluginInit :: TcPluginM Init PluginDefs
pluginInit = do
  typesModule   <- findTypesModule
  applySubTyCon <-                       tcLookupTyCon   =<< lookupOrig typesModule ( mkTcOcc   "ApplySub" )
  idTyCon       <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc "KId"      )
  bindTyCon     <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc "KBind"    )
  underTyCon    <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc "KUnder"   )
  extendTyCon   <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc "KExtend"  )
  composeTyCon  <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc ":*:"      )
  pure ( PluginDefs { .. } )

pluginSolve :: PluginDefs -> [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
pluginSolve _ _ _ = pure $ TcPluginOk [] []

pluginStop :: PluginDefs -> TcPluginM Stop ()
pluginStop _ = pure ()

--------------------------------------------------------------------------------
-- Simplification of type family applications.

pluginRewrite :: PluginDefs -> UniqFM TyCon TcPluginRewriter
pluginRewrite defs@( PluginDefs { applySubTyCon } ) =
  listToUFM
    [ ( applySubTyCon, rewriteSub defs ) ]

rewriteSub :: PluginDefs -> [ Ct ] -> [ Type ] -> TcPluginM Rewrite TcPluginRewriteResult 
rewriteSub defs@( PluginDefs { .. } ) givens applySubArgs
  | [ kϕ, kψ, k, subst, subst_arg ] <- applySubArgs
  = do
    tcPluginTrace "SystemF.Plugin rewrite {"
      ( ppr subst $$ ppr subst_arg )
    res <- rewritePlainSub kϕ kψ k subst subst_arg
    tcPluginTrace "SystemF.Plugin rewrite }" ( ppr res )
    pure ( finish res )
  | otherwise
  = pure $ TcPluginNoRewrite []
  where

    finish :: Maybe Type -> TcPluginRewriteResult
    finish Nothing      = TcPluginNoRewrite []
    finish ( Just res ) =
      TcPluginRewriteTo
        ( mkTyFamAppReduction "SystemF.Plugin" Nominal applySubTyCon applySubArgs res )
        []

    rewritePlainSub :: Type -> Type -> Type -> Type -> Type -> TcPluginM Rewrite ( Maybe Type )
    rewritePlainSub kϕ kψ k sub sub_arg =
      case cancelIdentities defs sub of
        ( Nothing, _ )
          -> pure ( Just sub_arg )
        ( Just sub', cancelOccurred )
          -> do
              mb_rewrite <- rewriteCancelledSub kϕ kψ k sub' sub_arg
              case mb_rewrite of
                Just rewrite
                  -> pure ( Just rewrite )
                Nothing
                  | cancelOccurred
                  -> pure ( Just $ mkTyConApp applySubTyCon [ kϕ, kψ, k, sub', sub_arg ] )
                  | otherwise
                  -> pure Nothing

    rewriteCancelledSub :: Type -> Type -> Type -> Type -> Type -> TcPluginM Rewrite ( Maybe Type )
    rewriteCancelledSub kϕ kψ k sub sub_arg
      -- (Clos) ApplySub t ( ApplySub s a )
      --   ===> ApplySub ( t :*: s ) a
      -- NB. might need to use Givens to find that the argument is 'ApplySub s a'.
      | Just ( kϕ0, _kϕ, _k, s, a ) <- detectApplySub defs givens sub_arg
      = let
          totalSub :: Type
          totalSub = case fst $ cancelIdentities defs s of
            Nothing -> sub
            Just s' ->
              mkTyConApp composeTyCon
                [ kϕ0, kϕ, kψ, sub, s' ]
        in pure . Just $
            mkTyConApp applySubTyCon [ kϕ0, kψ, k, totalSub, a ]
      -- (AssEnv) ApplySub ( t :*: ( s :*: r ) ) a
      --     ===> ApplySub ( ( t :*: s ) :*: r ) a
      | Just ( tc1, [ kϕ1, kψ1, kξ1, t, sr ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _  , kψ2, _  , s,  r ] ) <- splitTyConApp_maybe sr
      , tc2 == composeTyCon
      = do
        tcPluginTrace "SystemF.Plugin (AssEnv)" ( ppr sub $$ ppr sub_arg )
        pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp composeTyCon
                [ kϕ1, kψ2, kξ1
                , mkTyConApp composeTyCon [ kψ2, kψ1, kξ1, t, s ]
                , r
                ]
            , sub_arg
            ]
      -- (MapEnv) ApplySub ( t :*: KExtend s a ) b
      --     ===> ApplySub ( KExtend ( t :*: s ) ( ApplySub t a ) ) b
      -- NB: 's' could reduce to 'KId' here.
      | Just ( tc1, [ _  , kψ1, kξ1, t, ext ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ kϕ2, kψ2,   l, s,   a ] ) <- splitTyConApp_maybe ext
      , tc2 == extendTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp extendTyCon
              [ kϕ2, kψ, l
              , if isId defs givens s
                then t
                else mkTyConApp composeTyCon [ kϕ2, kψ2, kψ, t , s ]
              , mkTyConApp applySubTyCon [ kψ1, kξ1, l, t , a ]
              ]
            , sub_arg 
            ]
      -- (ShiftCons) ApplySub ( KExtend s a :*: KBind ) b
      --        ===> ApplySub s b
      -- NB: 's' could reduce to 'KId' here.
      | Just ( tc1, [_kϕ, _kψ0, _kψ, ext, bind] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [_kϕ0, _kψ0, _l, s,_a] ) <- splitTyConApp_maybe ext
      , tc2 == extendTyCon
      , Just ( tc3, _ ) <- splitTyConApp_maybe bind
      , tc3 == bindTyCon
      = pure . Just $
          if isId defs givens s
          then sub_arg
          else mkTyConApp applySubTyCon [kϕ, kψ, k, s, sub_arg]
      -- (ShiftLift1) ApplySub ( KUnder s :*: KBind ) a
      --         ===> ApplySub ( KBind :*: s ) a
      | Just ( tc1, [ _, _, _, under, bind ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, l ] ) <- splitTyConApp_maybe bind
      , tc2 == bindTyCon
      , Just ( tc3, [ _, kψ0, _, s ] ) <- splitTyConApp_maybe under
      , tc3 == underTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp composeTyCon
              [ kϕ, kψ0, kψ
              , mkTyConApp bindTyCon [ kψ0, l ]
              , s
              ]
            , sub_arg
            ]
      -- (ShiftLift2) ApplySub ( ( t :*: KUnder s ) :*: KBind ) a
      --         ===> ApplySub ( ( t :*: KBind ) :*: s ) a
      | Just ( tc1, [ _, _, _, t_under, bind ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, l ] ) <- splitTyConApp_maybe bind
      , tc2 == bindTyCon
      , Just ( tc3, [ _, kψ0, _, t, under ] ) <- splitTyConApp_maybe t_under
      , tc3 == composeTyCon
      , Just ( tc4, [ _, kψ1, _, s ] ) <- splitTyConApp_maybe under
      , tc4 == underTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp composeTyCon
               [ kϕ, kψ1, kψ
               , mkTyConApp composeTyCon
                  [ kψ1, kψ0, kψ
                  , t
                  , mkTyConApp bindTyCon [ kψ1, l ]
                  ]
               , s
               ]
            , sub_arg
            ]
      -- (Lift1) ApplySub ( KUnder t :*: KUnder s ) a
      --    ===> ApplySub ( KUnder ( t :*: s ) ) a
      | Just ( tc1, [ _, _, _, under_t, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [kϕ1, kψ1, l, t] ) <- splitTyConApp_maybe under_t
      , tc2 == underTyCon
      , Just ( tc3, [kϕ2, _kϕ1, _l, s] ) <- splitTyConApp_maybe under_s
      , tc3 == underTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp underTyCon
              [ kϕ2, kψ1, l
              , mkTyConApp composeTyCon [kϕ2, kϕ1, kψ1, t, s]
              ]
            , sub_arg
            ]
      -- (Lift2) ApplySub ( ( u :*: KUnder t ) :*: KUnder s ) a
      --    ===> ApplySub ( u :*: KUnder ( t :*: s ) ) a
      | Just ( tc1, [ _, _, _, u_under_t, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, kψ0, _, u, under_t ] ) <- splitTyConApp_maybe u_under_t
      , tc2 == composeTyCon
      , Just ( tc3, [ _, kψ2, _, t ] ) <- splitTyConApp_maybe under_t
      , tc3 == underTyCon
      , Just ( tc4, [ kϕ0, kψ1, l, s ] ) <- splitTyConApp_maybe under_s
      , tc4 == underTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp composeTyCon
              [ kϕ, kψ0, kψ
              , u
              , mkTyConApp underTyCon
                  [ kϕ0, kψ2, l
                  , mkTyConApp composeTyCon [ kϕ0, kψ1, kψ2, t , s ]
                  ]
              ]
            , sub_arg 
            ]
      -- (LiftEnv) ApplySub ( KExtend t a :*: KUnder s ) b
      --      ===> ApplySub ( KExtend ( t :*: s ) a ) b
      -- NB: 't' could reduce to 'KId' here.
      | Just ( tc1, [ _kϕ, _, _kψ, extend_t_a, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [_, kψ1, l, t,a] ) <- splitTyConApp_maybe extend_t_a
      , tc2 == extendTyCon
      , Just ( tc3, [kϕ0, kψ0, _, s] ) <- splitTyConApp_maybe under_s
      , tc3 == underTyCon
      = pure . Just $
          mkTyConApp applySubTyCon
            [ kϕ, kψ, k
            , mkTyConApp extendTyCon
                [ kϕ0, kψ1, l
                , if isId defs givens t
                  then s
                  else mkTyConApp composeTyCon [kϕ0, kψ0, kψ1, t, s]
                , a
                ]
            , sub_arg
            ]
      | otherwise
      = pure Nothing

detectApplySub :: PluginDefs -> [ Ct ] -> Type -> Maybe ( Type, Type, Type, Type, Type )
detectApplySub ( PluginDefs { applySubTyCon } ) =
  recognise \ ty ->
    case splitTyConApp_maybe ty of
      Just ( tc, [ kϕ, kψ, k, s, a ] )
        | tc == applySubTyCon
        -> Just ( kϕ, kψ, k, s, a )
      _ -> Nothing

recognise :: forall r. ( Type -> Maybe r ) -> [ Ct ] -> Type -> Maybe r
recognise f givens ty
  | Just r <- f ty
  = Just r
  | otherwise
  = go [ ty ] givens
  where
    go :: [ Type ] -> [ Ct ] -> Maybe r
    go _   [] = Nothing
    go tys ( g : gs )
      | EqPred NomEq lhs rhs <- classifyPredType ( ctPred g )
      = if
          | any ( eqType lhs ) tys
          -> case f rhs of
              Just r  -> Just r
              Nothing ->
                if any ( eqType rhs ) tys
                then go tys gs
                else go ( rhs : tys ) givens
          | any ( eqType rhs ) tys
          -> case f lhs of
              Just r  -> Just r
              Nothing ->
                if any ( eqType lhs ) tys
                then go tys gs
                else go ( lhs : tys ) givens
          | otherwise
          -> go tys gs
      | otherwise
      = go tys gs

isId :: PluginDefs -> [ Ct ] -> Type -> Bool
isId ( PluginDefs { .. } ) givens s = isJust $ recognise isIdTyCon givens s
  where
    isIdTyCon :: Type -> Maybe ()
    isIdTyCon ty = case splitTyConApp_maybe ty of
      Just ( tc, _ )
        | tc == idTyCon
        -> Just ()
      _ -> Nothing

cancelIdentities :: PluginDefs -> Type -> ( Maybe Type, Bool )
cancelIdentities ( PluginDefs { .. } ) = go False
  where
    go :: Bool -> Type -> ( Maybe Type, Bool )
    go cancel sub = case splitTyConApp_maybe sub of
      Just ( tc, args )
        | tc == idTyCon
        -> ( Nothing, cancel )
        | tc == underTyCon
        , [kϕ, kψ, k, a] <- args
        -> case go cancel a of
            ( Nothing, _ )
              -> ( Nothing, True )
            ( Just a', cancel' )
              -> ( Just $ mkTyConApp underTyCon [kϕ, kψ, k, a'], cancel' )
        | tc == extendTyCon
        , [kϕ, kψ, k, t, a] <- args
        ->
          let
            t' :: Type
            cancel' :: Bool
            ( t', cancel' ) = case go cancel t of
              ( Nothing , c' ) -> ( mkTyConApp idTyCon [ kϕ ], c' )
              ( Just res, c' ) -> ( res, c' )
          in 
            ( Just $ mkTyConApp extendTyCon [ kϕ, kψ, k, t', a ]
            , cancel'
            )
        | tc == composeTyCon
        , [kϕ, kψ, kξ, s, t] <- args
        -> case ( go cancel s, go cancel t) of
          ( ( Nothing, _  ), ( Nothing, _  ) ) -> ( Nothing, True )
          ( ( Just s', _  ), ( Nothing, _  ) ) -> ( Just s', True )
          ( ( Nothing, _  ), ( Just t', _  ) ) -> ( Just t', True )
          ( ( Just s', c1 ), ( Just t', c2 ) ) ->
            ( Just $ mkTyConApp composeTyCon [kϕ, kψ, kξ, s', t']
            , c1 || c2
            )
      _ -> ( Just sub, cancel )
