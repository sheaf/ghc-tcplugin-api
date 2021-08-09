{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: GHC.TcPlugin.API.Internal

This module provides operations to directly lift and unlift computations in
GHC's 'GHC.Tc.TcM' monad to the various type-checking plugin monads, in the form
of the functions

  > unsafeLiftTcM :: TcM a -> m a

  > unsafeWithRunInTcM :: ( ( forall a. m a -> TcM a ) -> TcM b ) -> m b

Here 'GHC.Tc.TcM' is GHC's internal type-checker monad.

It also exposes extra environment available in the solving/rewriting stages:

  > askRewriteEnv :: TcPluginM Rewrite RewriteEnv

  > askEvBinds :: TcPluginM Solve EvBindsVar

It is hoped that none of these internal operations are necessary, and that users
can fulfill their needs without importing this internal module.

Please file a bug on the issue tracker if you have encountered a situation
which requires the import of this module.

-}

module GHC.TcPlugin.API.Internal
  ( -- * Internal functions and types
    MonadTcPlugin(..), MonadTcPluginWork
  , unsafeLiftThroughTcM
    -- * Re-exported functions and types
  , TcPlugin(..), TcPluginStage(..)
  , TcPluginSolver
  , TcPluginM(..)
  , TcPluginErrorMessage(..)
  , TcPluginRewriter
  , askRewriteEnv
  , askDeriveds
  , askEvBinds
  , mkTcPlugin
  , mkTcPluginErrorTy
  )
  where

-- base
import Data.Kind
  ( Constraint, Type )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT(..) )

-- ghc
import qualified GHC.Builtin.Names
  as GHC.TypeLits
    ( errorMessageTypeErrorFamName
    , typeErrorTextDataConName
    , typeErrorAppendDataConName
    , typeErrorVAppendDataConName
    , typeErrorShowTypeDataConName
    )
import qualified GHC.Builtin.Types
  as GHC
    ( constraintKind )
import qualified GHC.Core.DataCon
  as GHC
    ( promoteDataCon )
import qualified GHC.Core.TyCon
  as GHC
    ( TyCon )
import qualified GHC.Core.TyCo.Rep
  as GHC
    ( PredType, Type(..), TyLit(..) )
import qualified GHC.Core.Type
  as GHC
    ( mkTyConApp, tcTypeKind )
import qualified GHC.Data.FastString
  as GHC
    ( fsLit )
import qualified GHC.Tc.Plugin
  as GHC
    ( tcLookupDataCon, tcLookupTyCon )
import qualified GHC.Tc.Types
  as GHC
    ( TcM, TcPlugin(..), TcPluginM
    , TcPluginSolver
#ifdef HAS_REWRITING
    , TcPluginRewriter
#else
    , getEvBindsTcPluginM
#endif
    , runTcPluginM, unsafeTcPluginTcM
    )
#ifdef HAS_REWRITING
import GHC.Tc.Types
    ( TcPluginSolveResult
    , TcPluginRewriteResult
    , RewriteEnv
    )
#endif
import qualified GHC.Tc.Types.Constraint
  as GHC
    ( Ct )
import qualified GHC.Tc.Types.Evidence
  as GHC
    ( EvBindsVar )
import qualified GHC.Types.Unique.FM
  as GHC
    ( UniqFM )

-- ghc-tcplugin-api
#ifndef HAS_REWRITING
import GHC.TcPlugin.API.Internal.Shim
  ( TcPluginSolveResult, TcPluginRewriteResult(..)
  , RewriteEnv
  , shimRewriter
  )
#endif

--------------------------------------------------------------------------------
-- Public types and functions.

-- | Stage of a type-checking plugin, used as a data kind.
data TcPluginStage
  = Init
  | Solve
  | Rewrite
  | Stop

-- | The @solve@ function of a type-checking plugin takes in Given and Wanted
-- constraints, and should return a 'GHC.Tc.Types.TcPluginSolveResult'
-- indicating which Wanted constraints it could solve, or whether any are
-- insoluble.
type TcPluginSolver
  =  [GHC.Ct] -- ^ Givens
  -> [GHC.Ct] -- ^ Wanteds
  -> TcPluginM Solve TcPluginSolveResult

-- | For rewriting type family applications, a type-checking plugin provides
-- a function of this type for each type family 'GHC.Core.TyCon.TyCon'.
--
-- The function is provided with the current set of Given constraints, together
-- with the arguments to the type family.
-- The type family application will always be fully saturated.
type TcPluginRewriter
  =  [GHC.Ct]     -- ^ Givens
  -> [GHC.Type]   -- ^ Type family arguments (saturated)
  -> TcPluginM Rewrite TcPluginRewriteResult

-- | A record containing all the stages necessary for the
-- operation of a type-checking plugin, as defined in this API.
--
-- __Note__: this is not the same record as GHC's built-in
-- 'GHC.Tc.Types.TcPlugin' record. Use 'mkTcPlugin' for the conversion.
--
-- To create a type-checking plugin, define something of this type
-- and then call 'mkTcPlugin' on the result.
-- This will return something that can be passed to 'GHC.Plugins.Plugin':
--
-- > plugin :: GHC.Plugins.Plugin
-- > plugin =
-- >   GHC.Plugins.defaultPlugin
-- >     { GHC.Plugins.tcPlugin =
-- >         \ args -> Just $
-- >            GHC.TcPlugin.API.mkTcPlugin ( myTcPlugin args )
-- >     }
-- >
-- > myTcPlugin :: [String] -> GHC.TcPlugin.API.TcPlugin
-- > myTcPlugin args = ...
data TcPlugin = forall s. TcPlugin
  { tcPluginInit    :: TcPluginM Init s
      -- ^ Initialise plugin, when entering type-checker.

  , tcPluginSolve   :: s -> TcPluginSolver
      -- ^ Solve some constraints.
      --
      -- This function will be invoked at two points in the constraint solving
      -- process: once to manipulate given constraints, and once to solve
      -- wanted constraints. In the first case (and only in the first case),
      -- no wanted constraints will be passed to the plugin.
      --
      -- The plugin can either return a contradiction,
      -- or specify that it has solved some constraints (with evidence),
      -- and possibly emit additional wanted constraints.
      --
      -- Use @ \\ _ _ _ -> pure $ TcPluginOK [] [] @ if your plugin
      -- does not provide this functionality.

  , tcPluginRewrite :: s -> GHC.UniqFM GHC.TyCon TcPluginRewriter
    -- ^ Rewrite saturated type family applications.
    --
    -- The plugin is expected to supply a mapping from type family names to
    -- rewriting functions. For each type family 'GHC.Core.TyCon.TyCon',
    -- the plugin should provide a function which takes in the given constraints
    -- and arguments of a saturated type family application, and return
    -- a possible rewriting.
    -- See 'TcPluginRewriter' for the expected shape of such a function.
    --
    -- Use @ const emptyUFM @ if your plugin does not provide this functionality.

  , tcPluginStop    :: s -> TcPluginM Stop ()
   -- ^ Clean up after the plugin, when exiting the type-checker.
  }

-- | The monad used for a type-checker plugin, parametrised by
-- the 'TcPluginStage' of the plugin.
type TcPluginM :: TcPluginStage -> ( Type -> Type )
data family TcPluginM s
newtype instance TcPluginM Init a =
  TcPluginInitM { tcPluginInitM :: GHC.TcPluginM a }
  deriving newtype ( Functor, Applicative, Monad )
#ifdef HAS_DERIVEDS
newtype instance TcPluginM Solve a =
  TcPluginSolveM { tcPluginSolveM :: BuiltinDefs -> GHC.EvBindsVar -> [GHC.Ct] -> GHC.TcPluginM a }
  deriving ( Functor, Applicative, Monad )
    via ( ReaderT BuiltinDefs ( ReaderT GHC.EvBindsVar ( ReaderT [GHC.Ct] GHC.TcPluginM ) ) )
#else
newtype instance TcPluginM Solve a =
  TcPluginSolveM { tcPluginSolveM :: BuiltinDefs -> GHC.EvBindsVar -> GHC.TcPluginM a }
  deriving ( Functor, Applicative, Monad )
    via ( ReaderT BuiltinDefs ( ReaderT GHC.EvBindsVar GHC.TcPluginM ) )
#endif
newtype instance TcPluginM Rewrite a =
  TcPluginRewriteM { tcPluginRewriteM :: BuiltinDefs -> RewriteEnv -> GHC.TcPluginM a }
  deriving ( Functor, Applicative, Monad )
    via ( ReaderT BuiltinDefs ( ReaderT RewriteEnv GHC.TcPluginM ) )
newtype instance TcPluginM Stop a =
  TcPluginStopM { tcPluginStopM :: GHC.TcPluginM a }
  deriving newtype ( Functor, Applicative, Monad )

-- | Ask for the evidence currently gathered by the type-checker.
--
-- Only available in the solver part of the type-checking plugin.
askEvBinds :: TcPluginM Solve GHC.EvBindsVar
askEvBinds = TcPluginSolveM
  \ _defs
    evBinds
#ifdef HAS_DERIVEDS
    _deriveds
#endif
  -> pure evBinds

-- | Ask for the Derived constraints that the solver was provided with.
--
-- Always returns the empty list on GHC 9.4 or above.
askDeriveds :: TcPluginM Solve [GHC.Ct]
askDeriveds =
#ifdef HAS_DERIVEDS
  TcPluginSolveM \ _defs _evBinds deriveds -> pure deriveds
#else
  pure []
#endif

-- | Ask for the current rewriting environment.
--
-- Only available in the rewriter part of the type-checking plugin.
askRewriteEnv :: TcPluginM Rewrite RewriteEnv
askRewriteEnv = TcPluginRewriteM ( \ _ rewriteEnv -> pure rewriteEnv )

-- | A 'MonadTcPlugin' is essentially a reader monad over GHC's 'GHC.Tc.TcM' monad.
--
-- This means we have both a @lift@ and an @unlift@ operation,
-- similar to @MonadUnliftIO@ or @MonadBaseControl@.
--
-- See for instance 'unsafeLiftThroughTcM', which is an example of function that
-- one would not be able to write using only a @lift@ operation.
--
-- Note that you must import the internal module in order to access the methods.
-- Please report a bug if you find yourself needing this functionality.
type  MonadTcPlugin :: ( Type -> Type ) -> Constraint
class Monad m => MonadTcPlugin m where

  {-# MINIMAL liftTcPluginM, unsafeWithRunInTcM #-}

  -- N.B.: these methods are not re-exported from the main module.

  -- | Lift a computation from GHC's 'GHC.TcPluginM' monad.
  liftTcPluginM :: GHC.TcPluginM a -> m a

  -- | Lift a computation from the 'GHC.Tc.TcM' monad.
  unsafeLiftTcM :: GHC.TcM a -> m a
  unsafeLiftTcM = liftTcPluginM . GHC.unsafeTcPluginTcM

  -- | Unlift a computation from the 'GHC.Tc.TcM' monad.
  --
  -- If this type signature seems confusing, I recommend reading Alexis King's
  -- excellent blog post on @MonadBaseControl@:
  --
  -- <https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/ Demystifying MonadBaseControl>
  unsafeWithRunInTcM :: ( ( forall a. m a -> GHC.TcM a ) -> GHC.TcM b ) -> m b

instance MonadTcPlugin ( TcPluginM Init ) where
  liftTcPluginM = TcPluginInitM
  unsafeWithRunInTcM runInTcM
    = unsafeLiftTcM $ runInTcM
#ifdef HAS_REWRITING
      ( GHC.runTcPluginM . tcPluginInitM )
#else
      ( ( `GHC.runTcPluginM` ( error "tcPluginInit: cannot access EvBindsVar" ) ) . tcPluginInitM ) 
#endif
instance MonadTcPlugin ( TcPluginM Solve ) where
  liftTcPluginM  = TcPluginSolveM
#ifdef HAS_DERIVEDS
                 . ( \ ma _defs _evBinds _deriveds -> ma )
#else
                 . ( \ ma _defs _evBinds -> ma )
#endif
  unsafeWithRunInTcM runInTcM
    = TcPluginSolveM
      \ builtinDefs
        evBinds
#ifdef HAS_DERIVEDS
        deriveds
#endif
      ->
        GHC.unsafeTcPluginTcM $ runInTcM
#ifdef HAS_REWRITING
          ( GHC.runTcPluginM
#ifdef HAS_DERIVEDS
          . ( \ f -> f builtinDefs evBinds deriveds )
#else
          . ( \ f -> f builtinDefs evBinds )
#endif
          . tcPluginSolveM )
#else
          ( ( `GHC.runTcPluginM` evBinds )
          . ( \ f -> f builtinDefs evBinds deriveds )
          . tcPluginSolveM
          )
#endif
instance MonadTcPlugin ( TcPluginM Rewrite ) where
  liftTcPluginM = TcPluginRewriteM . ( \ ma _ _ -> ma )
  unsafeWithRunInTcM runInTcM
    = TcPluginRewriteM \ builtinDefs rewriteEnv ->
      GHC.unsafeTcPluginTcM $ runInTcM
#ifdef HAS_REWRITING
        ( GHC.runTcPluginM
#else
        ( ( `GHC.runTcPluginM` ( error "tcPluginRewrite: cannot access EvBindsVar" ) )
#endif
        . ( \ f -> f builtinDefs rewriteEnv )
        . tcPluginRewriteM )
instance MonadTcPlugin ( TcPluginM Stop ) where
  liftTcPluginM = TcPluginStopM
  unsafeWithRunInTcM runInTcM
    = unsafeLiftTcM $ runInTcM 
#ifdef HAS_REWRITING
      ( GHC.runTcPluginM . tcPluginStopM )
#else
      ( ( `GHC.runTcPluginM` ( error "tcPluginStop: cannot access EvBindsVar" ) ) . tcPluginStopM )
#endif

-- | Take a function whose argument and result types are both within the 'GHC.Tc.TcM' monad,
-- and return a function that works within a type-checking plugin monad.
--
-- Please report a bug if you find yourself needing to use this function.
unsafeLiftThroughTcM :: MonadTcPlugin m => ( GHC.TcM a -> GHC.TcM b ) -> m a -> m b
unsafeLiftThroughTcM f ma = unsafeWithRunInTcM \ runInTcM -> f ( runInTcM ma )

-- | Use this function to create a type-checker plugin to pass to GHC.
mkTcPlugin
  :: TcPlugin     -- ^ type-checking plugin written with this library
  -> GHC.TcPlugin -- ^ type-checking plugin for GHC
mkTcPlugin ( TcPlugin
              { tcPluginInit = tcPluginInit :: TcPluginM Init userDefs
              , tcPluginSolve
              , tcPluginRewrite
              , tcPluginStop
              }
           ) =
  GHC.TcPlugin
    { GHC.tcPluginInit    = adaptUserInit    tcPluginInit
#ifdef HAS_REWRITING
    , GHC.tcPluginSolve   = adaptUserSolve   tcPluginSolve
    , GHC.tcPluginRewrite = adaptUserRewrite tcPluginRewrite
#else
    , GHC.tcPluginSolve   = adaptUserSolveAndRewrite
                              tcPluginSolve tcPluginRewrite
#endif
    , GHC.tcPluginStop    = adaptUserStop    tcPluginStop
    }
  where
    adaptUserInit :: TcPluginM Init userDefs -> GHC.TcPluginM ( TcPluginDefs userDefs )
    adaptUserInit userInit = do
      tcPluginBuiltinDefs <- initBuiltinDefs
      tcPluginUserDefs    <- tcPluginInitM userInit
      pure ( TcPluginDefs { tcPluginBuiltinDefs, tcPluginUserDefs })

#ifdef HAS_REWRITING
    adaptUserSolve :: ( userDefs -> TcPluginSolver )
                   -> TcPluginDefs userDefs
                   -> GHC.EvBindsVar
                   -> GHC.TcPluginSolver
    adaptUserSolve userSolve ( TcPluginDefs { tcPluginUserDefs, tcPluginBuiltinDefs } )
     evBindsVar
#ifdef HAS_DERIVEDS
      = \ givens deriveds wanteds -> do
        tcPluginSolveM ( userSolve tcPluginUserDefs givens wanteds )
          tcPluginBuiltinDefs evBindsVar deriveds
#else
      = \ givens _deriveds wanteds -> do
        tcPluginSolveM ( userSolve tcPluginUserDefs givens wanteds )
          tcPluginBuiltinDefs evBindsVar
#endif

    adaptUserRewrite :: ( userDefs -> GHC.UniqFM GHC.TyCon TcPluginRewriter )
                     -> TcPluginDefs userDefs -> GHC.UniqFM GHC.TyCon GHC.TcPluginRewriter
    adaptUserRewrite userRewrite ( TcPluginDefs { tcPluginUserDefs, tcPluginBuiltinDefs })
      = fmap
          ( \ userRewriter rewriteEnv givens tys ->
            tcPluginRewriteM ( userRewriter givens tys ) tcPluginBuiltinDefs rewriteEnv
          )
          ( userRewrite tcPluginUserDefs )
#else
    adaptUserSolveAndRewrite
      :: ( userDefs -> TcPluginSolver )
      -> ( userDefs -> GHC.UniqFM GHC.TyCon TcPluginRewriter )
      -> TcPluginDefs userDefs
      -> GHC.TcPluginSolver
    adaptUserSolveAndRewrite userSolve userRewrite ( TcPluginDefs { tcPluginUserDefs, tcPluginBuiltinDefs } )
      = \ givens deriveds wanteds -> do
        evBindsVar <- GHC.getEvBindsTcPluginM
        shimRewriter
          givens deriveds wanteds
          ( fmap
              ( \ userRewriter rewriteEnv gs tys ->
                tcPluginRewriteM ( userRewriter gs tys )
                  tcPluginBuiltinDefs rewriteEnv
              )
              ( userRewrite tcPluginUserDefs )
          )
          ( \ gs ds ws ->
            tcPluginSolveM ( userSolve tcPluginUserDefs gs ws )
              tcPluginBuiltinDefs evBindsVar ds
          )
#endif

    adaptUserStop :: ( userDefs -> TcPluginM Stop () ) -> TcPluginDefs userDefs -> GHC.TcPluginM ()
    adaptUserStop userStop ( TcPluginDefs { tcPluginUserDefs } ) =
      tcPluginStopM $ userStop tcPluginUserDefs

-- | Monads for type-checking plugins which are able to emit new constraints
-- and throw errors.
--
-- These operations are supported by the monads that 'tcPluginSolve'
-- and 'tcPluginRewrite' use; it is not possible to emit work or
-- throw type errors in 'tcPluginInit' or 'tcPluginStop'.
--
-- See 'mkTcPluginErrorTy' and 'GHC.TcPlugin.API.emitWork' for functions
-- which require this typeclass.
type  MonadTcPluginWork :: ( Type -> Type ) -> Constraint
class MonadTcPlugin m => MonadTcPluginWork m where
  {-# MINIMAL #-} -- to avoid the methods appearing in the haddocks
  askBuiltins :: m BuiltinDefs
  askBuiltins = error "askBuiltins: no default implementation"
instance MonadTcPluginWork ( TcPluginM Solve ) where
  askBuiltins = TcPluginSolveM
    \ builtinDefs
      _evBinds
#ifdef HAS_DERIVEDS
      _deriveds
#endif
    -> pure builtinDefs
instance MonadTcPluginWork ( TcPluginM Rewrite ) where
  askBuiltins = TcPluginRewriteM \ builtinDefs _evBinds -> pure builtinDefs

instance TypeError ( 'Text "Cannot emit new work in 'tcPluginInit'." )
      => MonadTcPluginWork ( TcPluginM Init ) where
  askBuiltins = error "Cannot emit new work in 'tcPluginInit'."
instance TypeError ( 'Text "Cannot emit new work in 'tcPluginStop'." )
      => MonadTcPluginWork ( TcPluginM Stop ) where
  askBuiltins = error "Cannot emit new work in 'tcPluginStop'."

-- | Use this type like 'GHC.TypeLits.ErrorMessage' to write an error message.
-- This error message can then be thrown at the type-level by the plugin,
-- by emitting a wanted constraint whose predicate is obtained from 'mkTcPluginErrorTy'.
-- 
-- A 'GHC.Tc.Types.Constraint.CtLoc' will still need to be provided in order to inform GHC of the
-- origin of the error (e.g.: which part of the source code should be
-- highlighted?). See 'GHC.TcPlugin.API.setCtLocM'.
data TcPluginErrorMessage
  = Txt !String
  -- ^ Show the text as is.
  | PrintType !GHC.Type
  -- ^ Pretty print the given type.
  | (:|:) !TcPluginErrorMessage !TcPluginErrorMessage
  -- ^ Put two messages side by side.
  | (:-:) !TcPluginErrorMessage !TcPluginErrorMessage
  -- ^ Stack two messages vertically.
infixl 5 :|:
infixl 6 :-:

-- | Create an error type with the desired error message.
--
-- The result can be paired with a 'GHC.Tc.Types.Constraint.CtLoc' in order to throw a type error,
-- for instance by using 'GHC.TcPlugin.API.newWanted'.
mkTcPluginErrorTy :: MonadTcPluginWork m => TcPluginErrorMessage -> m GHC.PredType
mkTcPluginErrorTy msg = do
  builtinDefs@( BuiltinDefs { typeErrorTyCon } ) <- askBuiltins
  let
    errorMsgTy :: GHC.PredType
    errorMsgTy = interpretErrorMessage builtinDefs msg
  pure $ GHC.mkTyConApp typeErrorTyCon [ GHC.constraintKind, errorMsgTy ]

--------------------------------------------------------------------------------
-- Private types and functions.
-- Not exposed at all, even from the internal module.

data BuiltinDefs =
  BuiltinDefs
    { typeErrorTyCon :: !GHC.TyCon
    , textTyCon      :: !GHC.TyCon
    , showTypeTyCon  :: !GHC.TyCon
    , concatTyCon    :: !GHC.TyCon
    , vcatTyCon      :: !GHC.TyCon
    }

data TcPluginDefs s
  = TcPluginDefs
  { tcPluginBuiltinDefs :: !BuiltinDefs
  , tcPluginUserDefs    :: !s
  }

initBuiltinDefs :: GHC.TcPluginM BuiltinDefs
initBuiltinDefs = do
  typeErrorTyCon  <-                        GHC.tcLookupTyCon   GHC.TypeLits.errorMessageTypeErrorFamName
  textTyCon       <- GHC.promoteDataCon <$> GHC.tcLookupDataCon GHC.TypeLits.typeErrorTextDataConName
  showTypeTyCon   <- GHC.promoteDataCon <$> GHC.tcLookupDataCon GHC.TypeLits.typeErrorShowTypeDataConName
  concatTyCon     <- GHC.promoteDataCon <$> GHC.tcLookupDataCon GHC.TypeLits.typeErrorAppendDataConName
  vcatTyCon       <- GHC.promoteDataCon <$> GHC.tcLookupDataCon GHC.TypeLits.typeErrorVAppendDataConName
  pure ( BuiltinDefs { .. } )

interpretErrorMessage :: BuiltinDefs -> TcPluginErrorMessage -> GHC.PredType
interpretErrorMessage ( BuiltinDefs { .. } ) = go
  where
    go :: TcPluginErrorMessage -> GHC.PredType
    go ( Txt str ) =
      GHC.mkTyConApp textTyCon [ GHC.LitTy . GHC.StrTyLit . GHC.fsLit $ str ]
    go ( PrintType ty ) =
      GHC.mkTyConApp showTypeTyCon [ GHC.tcTypeKind ty, ty ]
        -- The kind gets ignored by GHC when printing the error message (see GHC.Core.Type.pprUserTypeErrorTy).
        -- However, including the wrong kind can lead to ASSERT failures, so we compute the kind and pass it.
    go ( msg1 :|: msg2 ) =
      GHC.mkTyConApp concatTyCon [ go msg1, go msg2 ]
    go ( msg1 :-: msg2 ) =
      GHC.mkTyConApp vcatTyCon [ go msg1, go msg2 ]
