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

-- N.B. The qualified imports here are for clarity of exposition only.
-- In practice, I would recommend importing 'GHC.TcPlugin.API' unqualified.

-- Plugins must define "plugin :: GHC.Plugin", much like executables
-- must define "main :: IO ()".
plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.tcPlugin        = \ _args -> Just $ API.mkTcPlugin tcPlugin
    , GHC.pluginRecompile = GHC.purePlugin
    }

-- The type-checking plugin itself: specify the four stages.
tcPlugin :: API.TcPlugin
tcPlugin =
  API.TcPlugin
    { API.tcPluginInit    = tcPluginInit
    , API.tcPluginSolve   = tcPluginSolve
    , API.tcPluginRewrite = tcPluginRewrite
    , API.tcPluginStop    = tcPluginStop
    }

-- Definitions used by the plugin.
data PluginDefs =
  PluginDefs
    { natType          :: !API.TcType
    , zeroTyCon        :: !API.TyCon
    , succTyCon        :: !API.TyCon
    , badNatTyCon      :: !API.TyCon
    , addTyCon         :: !API.TyCon
    , cancellableClass :: !API.Class
    }

-- Look-up a module in a package, using their names.
findModule :: API.MonadTcPlugin m => String -> String -> m API.Module
findModule pkg modName = do
  findResult <- API.findImportedModule ( API.mkModuleName modName ) ( Just $ API.fsLit pkg )
  case findResult of
    API.Found _ res     -> pure res
    API.FoundMultiple _ -> error $ "RewriterPlugin: found multiple modules named " <> modName <> "."
    _                   -> error $ "RewriterPlugin: could not find any module named " <> modName <> "."

-- Initialise plugin state.
tcPluginInit :: API.TcPluginM API.Init PluginDefs
tcPluginInit = do
  defsModule       <- findModule "RewriterPlugin" "RewriterPlugin.Definitions"
  natType          <- fmap ( `API.mkTyConApp` [] ) . API.tcLookupTyCon   =<< API.lookupOrig defsModule ( API.mkTcOcc   "Nat"         )
  zeroTyCon        <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule ( API.mkDataOcc "Zero"        )
  succTyCon        <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule ( API.mkDataOcc "Succ"        )
  badNatTyCon      <- fmap API.promoteDataCon      . API.tcLookupDataCon =<< API.lookupOrig defsModule ( API.mkDataOcc "BadNat"      )
  addTyCon         <-                                API.tcLookupTyCon   =<< API.lookupOrig defsModule ( API.mkTcOcc   "Add"         )
  cancellableClass <-                                API.tcLookupClass   =<< API.lookupOrig defsModule ( API.mkClsOcc  "Cancellable" )
  pure ( PluginDefs { .. } )

-- The plugin does no constraint-solving, only type-family rewriting.
tcPluginSolve :: PluginDefs -> [ API.Ct ] -> [ API.Ct ] -> API.TcPluginM API.Solve API.TcPluginSolveResult
tcPluginSolve _ _ _ = pure $ API.TcPluginOk [] []

-- Nothing to shutdown.
tcPluginStop :: PluginDefs -> API.TcPluginM API.Stop ()
tcPluginStop _ = pure ()

--------------------------------------------------------------------------------
-- Simplification of type family applications.

-- Rewriting: we are only rewriting the 'Add' type family.
tcPluginRewrite :: PluginDefs -> API.UniqFM API.TyCon API.TcPluginRewriter
tcPluginRewrite defs@( PluginDefs { addTyCon } ) =
  API.listToUFM
    [ ( addTyCon, rewrite_add defs ) ]
    -- Each type family has its own rewriting function.
    -- Here we pass the rewrite_add function to rewrite the 'Add' type family.

-- Rewrite 'Add a b'.
rewrite_add :: PluginDefs -> [ API.Ct ] -> [ API.TcType ] -> API.TcPluginM API.Rewrite API.TcPluginRewriteResult
rewrite_add pluginDefs@( PluginDefs { .. } ) _givens tys
  | [a,b] <- tys
  = if
      -- Cancelling zero: "Add Zero b = b", emitting a "Cancellable b" Wanted constraint.
      | Just ( zero, [] ) <- API.splitTyConApp_maybe a
      , zero == zeroTyCon
      -> do
          wanted <- mkCancellableWanted pluginDefs b
          pure $ API.TcPluginRewriteTo
                  ( API.mkTyFamAppReduction "RewriterPlugin" API.Nominal addTyCon tys b )
                  [ wanted ]
      -- "Add a Zero = a", emitting a "Cancellable a" Wanted constraint.
      | Just ( zero, [] ) <- API.splitTyConApp_maybe b
      , zero == zeroTyCon
      -> do
        wanted <- mkCancellableWanted pluginDefs a
        pure $ API.TcPluginRewriteTo
                  ( API.mkTyFamAppReduction "RewriterPlugin" API.Nominal addTyCon tys a )
                  [ wanted ]

      -- Erroring on 'BadNat'.
      -- Add "BadNat b = BadNat", throwing an extra type error.
      | Just ( badNat, [] ) <- API.splitTyConApp_maybe a
      , badNat == badNatTyCon
      -> throwTypeError badRedn $
            Txt "RewriterPlugin detected a BadNat in the first argument of (+):"
              :-:
            PrintType a
      -- "Add a BadNat = BadNat", throwing an extra type error.
      | Just ( badNat, [] ) <- API.splitTyConApp_maybe b
      , badNat == badNatTyCon
      -> throwTypeError badRedn $
            Txt "RewriterPlugin detected a BadNat in the second argument of (+):"
              :-:
            PrintType b
      -- No rewriting otherwise.
      | otherwise
      -> pure API.TcPluginNoRewrite
  | otherwise
  = pure API.TcPluginNoRewrite
  where
    badRedn :: API.Reduction
    badRedn = API.mkTyFamAppReduction "RewriterPlugin" API.Nominal
      addTyCon tys (API.mkTyConApp badNatTyCon [])

-- Given the type "a", constructs a "Cancellable a" constraint
-- which has the source location information obtained from the rewriter environment.
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

-- Return the given type family reduction, while emitting an additional type error with the given message.
throwTypeError :: API.Reduction -> API.TcPluginErrorMessage -> API.TcPluginM API.Rewrite API.TcPluginRewriteResult
throwTypeError badRedn msg = do
  env <- API.askRewriteEnv
  errorTy <- API.mkTcPluginErrorTy msg
  let
    errorCtLoc :: API.CtLoc
    errorCtLoc = API.bumpCtLocDepth $ API.rewriteEnvCtLoc env
  errorCtEv <- API.setCtLocM errorCtLoc $ API.newWanted errorCtLoc errorTy
  pure $ API.TcPluginRewriteTo badRedn [ API.mkNonCanonical errorCtEv ]
