{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module SystemF.Plugin ( plugin ) where

-- base
import Data.Functor
  ( ($>) )

-- transformers
import Control.Monad.Trans.State.Strict
  ( State, StateT )
import qualified Control.Monad.Trans.State.Strict as State
  ( runState, runStateT, get, put, modify )
import Control.Monad.Trans.Class
  ( lift )

-- ghc
import qualified GHC.Plugins as GHC
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Utils.Outputable
  ( empty, ($$) )

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
  let modlName = mkModuleName "SystemF.Type"
  pkgQual    <- resolveImport      modlName Nothing
  findResult <- findImportedModule modlName pkgQual
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
  composeTyCon  <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig typesModule ( mkDataOcc ":.:"      )
  pure ( PluginDefs { .. } )

pluginSolve :: PluginDefs -> [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
pluginSolve _ _ _ = pure $ TcPluginOk [] []

pluginStop :: PluginDefs -> TcPluginM Stop ()
pluginStop _ = pure ()

--------------------------------------------------------------------------------
-- Simplification of type family applications.

type RewriteQ = Bool
pattern NoReduction :: RewriteQ
pattern NoReduction = False
pattern DidRewrite :: RewriteQ
pattern DidRewrite = True

type RewriteM = StateT ( RewriteQ, [ Coercion ] ) ( TcPluginM Rewrite )

runRewriteM :: RewriteM a -> TcPluginM Rewrite ( Maybe ( a, [ Coercion ] ) )
runRewriteM ma = do
  ( a, ( didRewrite, deps ) ) <- State.runStateT ma ( NoReduction, [] )
  pure $
    if didRewrite
    then Just ( a, deps )
    else Nothing

rewrote :: [ Coercion ] -> RewriteM ()
rewrote deps = State.modify ( \ ( _, deps0 ) -> ( DidRewrite, deps0 ++ deps ) )

askIfRewrote :: RewriteM a -> RewriteM ( a, RewriteQ )
askIfRewrote ma = do
  ( before, deps1 ) <- State.get
  State.put ( NoReduction, [] )
  a <- ma
  ( didRewrite, deps2 ) <- State.get
  State.put
    ( before || didRewrite, deps1 ++ deps2 )
  pure ( a, didRewrite )

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
    res <- runRewriteM $ rewriteApplySub kϕ kψ k subst subst_arg
    tcPluginTrace "SystemF.Plugin rewrite }" ( ppr res )
    pure ( finish res )
  | otherwise
  = pure $ TcPluginNoRewrite
  where

    finish :: Maybe ( Type, [ Coercion ] ) -> TcPluginRewriteResult
    finish ( Just ( ty, deps ) ) =
      TcPluginRewriteTo
        ( mkTyFamAppReduction "SystemF.Plugin" Nominal deps applySubTyCon applySubArgs ty )
        []
    finish _ = TcPluginNoRewrite


    rewriteApplySub :: Type -> Type -> Type -> Type -> Type -> RewriteM Type
    rewriteApplySub kϕ kψ k sub sub_arg
      -- (Clos) ApplySub t ( ApplySub s a )
      --   ===> ApplySub ( t :.: s ) a
      -- NB. might need to use Givens to find that the argument is 'ApplySub s a'.
      | Just ( ( kϕ0, _kϕ, l, s, a ), deps ) <- detectApplySub defs givens sub_arg
      , let
          sub' :: Type
          sub' = mkTyConApp composeTyCon [ kϕ0, kϕ, kψ, sub, s ]
      = do
        rewrote deps
        rewriteApplySub kϕ0 kψ l sub' a
      | otherwise
      = do
        sub' <- canonicaliseSub defs givens kϕ kψ k sub
        case isId defs givens sub' of
          Just deps ->
            rewrote deps $> sub_arg
          Nothing ->
            pure $
              mkTyConApp applySubTyCon [ kϕ, kψ, k, sub', sub_arg ]

canonicaliseSub :: PluginDefs -> [ Ct ]
                -> Type -> Type -> Type -> Type
                -> RewriteM Type
canonicaliseSub defs@( PluginDefs { .. } ) givens kϕ kψ k = go

  where
    go :: Type -> RewriteM Type
    go sub
      -- (AssEnv) t :.: ( s :.: r )
      --     ===> ( t :.: s ) :.: r
      | Just ( tc1, [ kϕ1, kψ1, kξ1, t, sr ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _  , kψ2, _  , s,  r ] ) <- splitTyConApp_maybe sr
      , tc2 == composeTyCon
      = do
        lift $ tcPluginTrace "AssEnv" ( ppr t $$ ppr s $$ ppr r )
        rewrote []
        go $
          mkTyConApp composeTyCon
            [ kϕ1, kψ2, kξ1
            , mkTyConApp composeTyCon [ kψ2, kψ1, kξ1, t, s ]
            , r
            ]
      -- (MapEnv) t :.: KExtend s a
      --     ===> KExtend ( t :.: s ) ( ApplySub t a )
      | Just ( tc1, [ _  , kψ1, kξ1, t, ext ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ kϕ2, kψ2,   l, s,   a ] ) <- splitTyConApp_maybe ext
      , tc2 == extendTyCon
      = do
        lift $ tcPluginTrace "MapEnv" ( ppr t $$ ppr s )
        rewrote []
        go $
          mkTyConApp extendTyCon
            [ kϕ2, kψ, l
            , mkTyConApp composeTyCon [ kϕ2, kψ2, kψ, t , s ]
            , mkTyConApp applySubTyCon [ kψ1, kξ1, l, t , a ]
            ]
      -- (ShiftCons) KExtend s a :.: KBind
      --        ===> s
      | Just ( tc1, [_kϕ, _kψ0, _kψ, ext, bind] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [_kϕ0, _kψ0, _l, s,_a] ) <- splitTyConApp_maybe ext
      , tc2 == extendTyCon
      , Just ( tc3, _ ) <- splitTyConApp_maybe bind
      , tc3 == bindTyCon
      = do
        lift $ tcPluginTrace "ShiftCons" ( ppr s )
        rewrote []
        go s
      -- (ShiftLift1) KUnder s :.: KBind
      --         ===> KBind :.: s
      | Just ( tc1, [ _, _, _, under, bind ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, l ] ) <- splitTyConApp_maybe bind
      , tc2 == bindTyCon
      , Just ( tc3, [ _, kψ0, _, s ] ) <- splitTyConApp_maybe under
      , tc3 == underTyCon
      = do
        lift $ tcPluginTrace "ShiftLift1" ( ppr s )
        rewrote []
        go $
          mkTyConApp composeTyCon
            [ kϕ, kψ0, kψ
            , mkTyConApp bindTyCon [ kψ0, l ]
            , s
            ]
      -- (ShiftLift2) ( t :.: KUnder s ) :.: KBind
      --         ===> ( t :.: KBind ) :.: s
      | Just ( tc1, [ _, _, _, t_under, bind ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, l ] ) <- splitTyConApp_maybe bind
      , tc2 == bindTyCon
      , Just ( tc3, [ _, kψ0, _, t, under ] ) <- splitTyConApp_maybe t_under
      , tc3 == composeTyCon
      , Just ( tc4, [ _, kψ1, _, s ] ) <- splitTyConApp_maybe under
      , tc4 == underTyCon
      = do
        lift $ tcPluginTrace "ShiftLift2" ( ppr t $$ ppr s )
        rewrote []
        go $
          mkTyConApp composeTyCon
            [ kϕ, kψ1, kψ
            , mkTyConApp composeTyCon
               [ kψ1, kψ0, kψ
               , t
               , mkTyConApp bindTyCon [ kψ1, l ]
               ]
            , s
            ]
      -- (Lift1) KUnder t :.: KUnder s
      --    ===> KUnder ( t :.: s )
      | Just ( tc1, [ _, _, _, under_t, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [kϕ1, kψ1, l, t] ) <- splitTyConApp_maybe under_t
      , tc2 == underTyCon
      , Just ( tc3, [kϕ2, _kϕ1, _l, s] ) <- splitTyConApp_maybe under_s
      , tc3 == underTyCon
      = do
        lift $ tcPluginTrace "Lift1" ( ppr t $$ ppr s )
        rewrote []
        go $
          mkTyConApp underTyCon
          [ kϕ2, kψ1, l
          , mkTyConApp composeTyCon [kϕ2, kϕ1, kψ1, t, s]
          ]
      -- (Lift2) ( u :.: KUnder t ) :.: KUnder s
      --    ===> u :.: KUnder ( t :.: s )
      | Just ( tc1, [ _, _, _, u_under_t, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [ _, kψ0, _, u, under_t ] ) <- splitTyConApp_maybe u_under_t
      , tc2 == composeTyCon
      , Just ( tc3, [ _, kψ2, _, t ] ) <- splitTyConApp_maybe under_t
      , tc3 == underTyCon
      , Just ( tc4, [ kϕ0, kψ1, l, s ] ) <- splitTyConApp_maybe under_s
      , tc4 == underTyCon
      = do
        lift $ tcPluginTrace "Lift2" ( ppr u $$ ppr t $$ ppr s )
        rewrote []
        go $
          mkTyConApp composeTyCon
            [ kϕ, kψ0, kψ
            , u
            , mkTyConApp underTyCon
                [ kϕ0, kψ2, l
                , mkTyConApp composeTyCon [ kϕ0, kψ1, kψ2, t , s ]
                ]
            ]
      -- (LiftEnv) KExtend t a :.: KUnder s
      --      ===> KExtend ( t :.: s ) a
      | Just ( tc1, [ _kϕ, _, _kψ, extend_t_a, under_s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, [_, kψ1, l, t,a] ) <- splitTyConApp_maybe extend_t_a
      , tc2 == extendTyCon
      , Just ( tc3, [kϕ0, kψ0, _, s] ) <- splitTyConApp_maybe under_s
      , tc3 == underTyCon
      = do
        lift $ tcPluginTrace "LiftEnv" ( ppr t $$ ppr s )
        s' <-
          case isId defs givens t of
            Just deps -> do
              rewrote deps
              return s
            Nothing -> do
              rewrote []
              return $ mkTyConApp composeTyCon [kϕ0, kψ0, kψ1, t, s]
        go $
          mkTyConApp extendTyCon
            [ kϕ0, kψ1, l, s', a]
      -- (IdL) KId :.: s
      --  ===> s
      | Just ( tc1, [ _, _ , _, i, s ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, _ ) <- splitTyConApp_maybe i
      , tc2 == idTyCon
      = do
        lift $ tcPluginTrace "IdL" ( ppr s )
        rewrote []
        go s
      -- (IdR) s :.: KId
      --  ===> s
      | Just ( tc1, [ _, _, _, s, i ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      , Just ( tc2, _ ) <- splitTyConApp_maybe i
      , tc2 == idTyCon
      = do
        lift $ tcPluginTrace "IdR" ( ppr s )
        rewrote []
        go s
      -- (LiftId) KUnder KId
      --     ===> KId
      | Just ( tc1, [ _, _, _, i ] ) <- splitTyConApp_maybe sub
      , tc1 == underTyCon
      , Just ( tc2, _ ) <- splitTyConApp_maybe i
      , tc2 == idTyCon
      = do
        lift $ tcPluginTrace "LiftId" empty
        rewrote []
        pure $ i
      -- Recur under KUnder.
      | Just ( tc1, [ kϕ0, kψ0, k0, s ] ) <- splitTyConApp_maybe sub
      , tc1 == underTyCon
      = do
        lift $ tcPluginTrace "Recur: KUnder" ( ppr s )
        ( s', didRewrite ) <- askIfRewrote $ canonicaliseSub defs givens kϕ0 kψ0 k0 s
        let
          next :: Type -> RewriteM Type
          next = if didRewrite then go else pure
        next $
          mkTyConApp underTyCon [ kϕ0, kψ0, k0, s' ]
      -- Recur under KExtend.
      | Just ( tc1, [ kϕ0, kψ0, k0, s, a ] ) <- splitTyConApp_maybe sub
      , tc1 == extendTyCon
      = do
        lift $ tcPluginTrace "Recur: KExtend" ( ppr s )
        ( s', didRewrite ) <- askIfRewrote $ canonicaliseSub defs givens kϕ0 kψ0 k0 s
        let
          next :: Type -> RewriteM Type
          next = if didRewrite then go else pure
        next $
          mkTyConApp extendTyCon [ kϕ0, kψ0, k0, s', a ]
      -- Recur under composition.
      | Just ( tc1, [ kϕ0, kψ0, kξ0, s, t ] ) <- splitTyConApp_maybe sub
      , tc1 == composeTyCon
      = do
        lift $ tcPluginTrace "Recur: (:.:)" ( ppr s $$ ppr t )
        ( s', didRewrite1 ) <- askIfRewrote $ canonicaliseSub defs givens kψ0 kξ0 k s
        ( t', didRewrite2 ) <- askIfRewrote $ canonicaliseSub defs givens kϕ0 kψ0 k t
        let
          next :: Type -> RewriteM Type
          next = if didRewrite1 || didRewrite2 then go else pure
        next $
          mkTyConApp composeTyCon [ kϕ0, kψ0, kξ0, s', t' ]
      | otherwise
      = pure sub

detectApplySub :: PluginDefs -> [ Ct ] -> Type -> Maybe ( ( Type, Type, Type, Type, Type ), [ Coercion ] )
detectApplySub ( PluginDefs { applySubTyCon } ) =
  recognise \ ty ->
    case splitTyConApp_maybe ty of
      Just ( tc, [ kϕ, kψ, k, s, a ] )
        | tc == applySubTyCon
        -> Just ( kϕ, kψ, k, s, a )
      _ -> Nothing

-- | Recognise whether a given type is of the form expected by the provided function,
-- using Givens to rewrite the type if necessary.
--
-- Useful to check whether some type is a type-family application in the presence of
-- Givens.
recognise :: forall r. ( Type -> Maybe r ) -> [ Ct ] -> Type -> Maybe ( r, [ Coercion ] )
recognise f givens ty
  | Just r <- f ty
  = Just ( r, [] )
  | otherwise
  = case ( `State.runState` [] ) $ go [ ty ] givens of
      ( Nothing, _ ) -> Nothing
      ( Just ty', deps ) -> Just ( ty', deps )
  where
    go :: [ Type ] -> [ Ct ] -> State [ Coercion ] ( Maybe r )
    go _   [] = return Nothing
    go tys ( g : gs )
      | let ctTy = ctPred g
      , EqPred NomEq lhs rhs <- classifyPredType ctTy
      , let
          co = ctEvCoercion $ ctEvidence g
          declareDeps = State.modify ( \ deps -> co : deps )
      = if
          | any ( eqType lhs ) tys
          -> case f rhs of
              Just r  -> do
                declareDeps
                return $ Just r
              Nothing ->
                if any ( eqType rhs ) tys
                then do
                  declareDeps
                  go tys gs
                else go ( rhs : tys ) givens
          | any ( eqType rhs ) tys
          -> case f lhs of
              Just r  -> do
                declareDeps
                return $ Just r
              Nothing ->
                if any ( eqType lhs ) tys
                then do
                  declareDeps
                  go tys gs
                else go ( lhs : tys ) givens
          | otherwise
          -> go tys gs
      | otherwise
      = go tys gs

isId :: PluginDefs -> [ Ct ] -> Type -> Maybe [ Coercion ]
isId ( PluginDefs { .. } ) givens s = snd <$> recognise isIdTyCon givens s
  where
    isIdTyCon :: Type -> Maybe ()
    isIdTyCon ty = case splitTyConApp_maybe ty of
      Just ( tc, _ )
        | tc == idTyCon
        -> Just ()
      _ -> Nothing
