{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module RewriterPlugin ( plugin ) where

-- ghc
import qualified GHC.Plugins as GHC
  ( Plugin(..), defaultPlugin, purePlugin )

-- ghc-tcplugin-api
import qualified GHC.TcPlugin.API as API
import GHC.TcPlugin.API
  ( TcPluginErrorMessage(..) )

--------------------------------------------------------------------------------
-- Plugin definition and setup/finalisation.

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.tcPlugin        = \ _args -> Just $ API.mkTcPlugin tcPlugin
    , GHC.pluginRecompile = GHC.purePlugin
    }

tcPlugin :: API.TcPlugin
tcPlugin =
  API.TcPlugin
    { API.tcPluginInit    = tcPluginInit
    , API.tcPluginSolve   = tcPluginSolve
    , API.tcPluginRewrite = tcPluginRewrite
    , API.tcPluginStop    = tcPluginStop
    }

data PluginDefs =
  PluginDefs
    { natType          :: !API.TcType
    , zeroTyCon        :: !API.TyCon
    , succTyCon        :: !API.TyCon
    , badNatTyCon      :: !API.TyCon
    , addTyCon         :: !API.TyCon
    , cancellableClass :: !API.Class
    }

findModule :: API.MonadTcPlugin m => String -> String -> m API.Module
findModule pkg modName = do
  findResult <- API.findImportedModule ( API.mkModuleName modName ) ( Just $ API.fsLit pkg )
  case findResult of
    API.Found _ res     -> pure res
    API.FoundMultiple _ -> error $ "RewriterPlugin: found multiple modules named " <> modName <> "."
    _                   -> error $ "RewriterPlugin: could not find any module named " <> modName <> "."

tcPluginInit :: API.TcPluginM API.Init PluginDefs
tcPluginInit = do
  defsModule       <- findModule "RewriterPlugin" "RewriterPlugin.Definitions"
  natType          <- fmap ( `API.mkTyConApp` [] ) . API.tcLookupTyCon   =<< API.lookupOrig defsModule    ( API.mkTcOcc   "Nat"         )
  zeroTyCon        <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule    ( API.mkDataOcc "Zero"        )
  succTyCon        <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule    ( API.mkDataOcc "Succ"        )
  badNatTyCon      <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule    ( API.mkDataOcc "BadNat"      )
  addTyCon         <-                                API.tcLookupTyCon   =<< API.lookupOrig defsModule    ( API.mkTcOcc   "Add"         )
  cancellableClass <-                                API.tcLookupClass   =<< API.lookupOrig defsModule    ( API.mkClsOcc  "Cancellable" )
  pure ( PluginDefs { .. } )

tcPluginSolve :: PluginDefs -> [ API.Ct ] -> [ API.Ct ] -> API.TcPluginM API.Solve API.TcPluginSolveResult
tcPluginSolve _ _ _ = pure $ API.TcPluginOk [] []

tcPluginStop :: PluginDefs -> API.TcPluginM API.Stop ()
tcPluginStop _ = pure ()

--------------------------------------------------------------------------------
-- Simplification of type family applications.

tcPluginRewrite :: PluginDefs -> API.UniqFM API.TyCon API.TcPluginRewriter
tcPluginRewrite defs@( PluginDefs { addTyCon } ) =
  API.listToUFM
    [ ( addTyCon, rewrite_add defs ) ]

rewrite_add :: PluginDefs -> [ API.Ct ] -> [ API.TcType ] -> API.TcPluginM API.Rewrite API.TcPluginRewriteResult
rewrite_add pluginDefs@( PluginDefs { .. } ) _givens tys
  | [a,b] <- tys
  = if
      -- Cancelling zero.
      | Just ( zero, [] ) <- API.splitTyConApp_maybe a
      , zero == zeroTyCon
      -> do
          wanted <- mkCancellableWanted pluginDefs b
          pure $ API.TcPluginRewriteTo
                  ( API.mkTyFamAppReduction "RewriterPlugin" API.Nominal addTyCon tys b )
                  [ wanted ]
      | Just ( zero, [] ) <- API.splitTyConApp_maybe b
      , zero == zeroTyCon
      -> do
        wanted <- mkCancellableWanted pluginDefs a
        pure $ API.TcPluginRewriteTo
                  ( API.mkTyFamAppReduction "RewriterPlugin" API.Nominal addTyCon tys a )
                  [ wanted ]

      -- Erroring on 'BadNat'.
      | Just ( badNat, [] ) <- API.splitTyConApp_maybe a
      , badNat == badNatTyCon
      -> throwTypeError $
            Txt "RewriterPlugin detected a BadNat in the first argument of (+):"
              :-:
            PrintType a
      | Just ( badNat, [] ) <- API.splitTyConApp_maybe b
      , badNat == badNatTyCon
      -> throwTypeError $
            Txt "RewriterPlugin detected a BadNat in the second argument of (+):"
              :-:
            PrintType b
      -- No rewriting otherwise.
      | otherwise
      -> pure $ API.TcPluginNoRewrite []
  | otherwise
  = pure $ API.TcPluginNoRewrite []

mkCancellableWanted :: PluginDefs -> API.TcType -> API.TcPluginM API.Rewrite API.Ct
mkCancellableWanted ( PluginDefs { .. } ) ty = do
  env <- API.askRewriteEnv
  let
    ctLoc :: API.CtLoc
    ctLoc = API.bumpCtLocDepth $ API.rewriteEnvCtLoc env
    ctPredTy :: API.PredType
    ctPredTy = API.mkTyConApp ( API.classTyCon cancellableClass ) [ ty ]
  ctEv <- API.setCtLocM ctLoc $ API.newWanted ctLoc ctPredTy
  pure ( API.mkNonCanonical ctEv )

throwTypeError :: API.TcPluginErrorMessage -> API.TcPluginM API.Rewrite API.TcPluginRewriteResult
throwTypeError msg = do
  env <- API.askRewriteEnv
  errorTy <- API.mkTcPluginErrorTy msg
  let
    errorCtLoc :: API.CtLoc
    errorCtLoc = API.bumpCtLocDepth $ API.rewriteEnvCtLoc env
  errorCtEv <- API.setCtLocM errorCtLoc $ API.newWanted errorCtLoc errorTy
  pure $ API.TcPluginNoRewrite [ API.mkNonCanonical errorCtEv ]
