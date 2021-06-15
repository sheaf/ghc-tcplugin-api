{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: GHC.TcPlugin.API
Description: An interface for writing type-checking plugins.

This module provides a slightly higher-level monadic interface for writing
type-checking plugins for GHC. It attempts to re-export all the functionality
from GHC that is relevant to plugin authors, as well as providing utility
functions to streamline certain common operations such as creating evidence
(to solve constraints) or custom type errors (for insoluble constraints).

Call 'mkTcPlugin' to create a GHC type-checking plugin from the data of a
'TcPlugin' as defined by this library.

The internal module "GHC.TcPlugin.API.Internal" can be used to directly
lift and unlift computations in GHC's 'TcM' monad, but it is hoped that
the interface provided in this module is sufficient.

Please file a bug on the bug tracker if you have found yourself needing
to use the internal module.
-}

module GHC.TcPlugin.API
  ( -- * Basic TcPluginM functionality
    mkTcPlugin, tcPluginIO
  , TcPlugin(..), TcPluginStage(..), TcPluginM
  , MonadTcPlugin, MonadTcPluginTypeError
  , TcPluginMonadStage, TcPluginTypeErrorStage
  , TcPluginErrorMessage(..)
  , TcPluginSolver, TcPluginSolveResult(..)
  , TcPluginRewriter, TcPluginRewriteResult(..)

    -- * Finding Modules and Names
  , findImportedModule
  , Module, ModuleName, FindResult(..)
  , lookupOrig, mkModuleName
  , FastString, fsLit

    -- * Manipulating constraints
  , Ct(..), CtLoc(..), CtEvidence(..), CtOrigin(..)
  , PredType, EvExpr
  , newWanted, newDerived, newGiven
  , bumpCtLocDepth, setCtLocM, setCtLocRewriteM
  , mkNonCanonical

    -- * Analysing types, constraints & predicates
  , Pred(..), EqRel(..), FunDep
  , eqType
  , ctPred, ctLoc, ctEvidence
  , ctFlavour, ctEqRel, ctOrigin
  , classifyPredType
  , mkClassPred, getClassPredTys_maybe

    -- * Manipulating coercions
  , mkPluginUnivCo
  , Role(..), UnivCoProvenance(..)
  , newCoercionHole
  , mkPrimEqPredRole, mkUnivCo

    -- * Rewriting type family applications
  , mkTyFamAppReduction, askRewriteEnv
  , UniqFM, Reduction(..), RewriteEnv(..)
  , matchFam
  , emptyUFM, listToUFM
  , mkTyConTy, mkTyConApp, splitTyConApp_maybe
  , mkAppTy, mkAppTys

    -- * Looking up Names in the typechecking environment
  , Name, OccName(..), TyThing(..), TcTyThing(..)
  , Class(..), DataCon, TyCon(..), Id
  , tcLookupGlobal
  , tcLookupTyCon
  , tcLookupDataCon
  , tcLookupClass
  , tcLookup
  , tcLookupId
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc
  , promoteDataCon

    -- * Getting the TcM state
  , HscEnv(..), TcGblEnv(..), TcLclEnv(..)
  , InstEnvs(..), FamInstEnv
  , getTopEnv
  , getEnvs
  , getInstEnvs
  , getFamInstEnvs

    -- * Type variables
  , newUnique
  , newFlexiTyVar
  , isTouchableTcPluginM
  , TcType, TcTyVar, Unique, Kind
  , mkTyVarTy, mkTyVarTys
  , getTyVar_maybe

    -- * Functions
  , AnonArgFlag(..), Mult
  , mkFunTy, mkVisFunTy, mkInvisFunTy, mkVisFunTys
  , mkForAllTy, mkForAllTys, mkInvisForAllTys
  , mkPiTy, mkPiTys
  , mkFunTyMany
  , mkScaledFunTy
  , mkVisFunTyMany, mkVisFunTysMany
  , mkInvisFunTyMany, mkInvisFunTysMany

    -- * Zonking
  , zonkTcType
  , zonkCt

    -- * Manipulating evidence bindings
  , askEvBinds, mkPluginUnivEvTerm
  , EvBind(..), EvTerm(..), EvVar, CoercionHole(..)
  , newEvVar, setEvBind, evCoercion

    -- * Displaying messages
  , tcPluginTrace
  , mkTcPluginErrorTy
  , Outputable(..), SDoc

    -- * Built-in types
  , module GHC.Builtin.Types
  )
  where

-- base
import Data.Kind
  ( Constraint )

-- ghc
import GHC.Builtin.Types
import GHC.Core.Class
  ( Class(..), FunDep )
import GHC.Core.Coercion
  ( Reduction(..)
  , mkUnivCo, mkPrimEqPredRole
  )
import GHC.Core.Coercion.Axiom
  ( Role(..) )
import GHC.Core.DataCon
  ( DataCon
  , promoteDataCon
  )
import GHC.Core.InstEnv
  ( InstEnvs(..) )
import GHC.Core.FamInstEnv
  ( FamInstEnv )
import GHC.Core.Predicate
  ( Pred(..), EqRel(..)
  , classifyPredType
  , mkClassPred, getClassPredTys_maybe
  )
import GHC.Core.TyCon
  ( TyCon(..) )
import GHC.Core.TyCo.Rep
import GHC.Core.Type
  ( eqType, mkTyConTy, mkTyConApp, splitTyConApp_maybe
  , mkAppTy, mkAppTys, getTyVar_maybe
  )
import GHC.Data.FastString
  ( FastString, fsLit )
import GHC.Driver.Env.Types
  ( HscEnv(..) )
import qualified GHC.Tc.Plugin
  as GHC
import GHC.Tc.Types
  ( TcPluginSolveResult(..), TcPluginRewriteResult(..)
  , TcTyThing(..), TcGblEnv(..), TcLclEnv(..)
  , RewriteEnv(..)
  )
import GHC.Tc.Types.Constraint
  ( Ct(..), CtLoc(..), CtEvidence(..)
  , ctPred, ctLoc, ctEvidence
  , ctFlavour, ctEqRel, ctOrigin
  , bumpCtLocDepth
  , mkNonCanonical
  )
import GHC.Tc.Types.Evidence
  ( EvBind(..), EvTerm(..), EvExpr
  , evCoercion
  )
import GHC.Tc.Types.Origin
  ( CtOrigin(..) )
import qualified GHC.Tc.Utils.Monad
  as GHC
    ( traceTc, setCtLocM )
import GHC.Tc.Utils.TcType
  ( TcType )
import GHC.Types.Name
  ( Name )
import GHC.Types.Name.Occurrence
  ( OccName(..)
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc
  )
import GHC.Types.TyThing
  ( TyThing(..) )
import GHC.Types.Unique
  ( Unique )
import GHC.Types.Unique.FM
  ( UniqFM, emptyUFM, listToUFM )
import GHC.Types.Var
  ( Id, TcTyVar, EvVar )
import GHC.Utils.Outputable
  ( Outputable(..), SDoc )
import GHC.Unit.Finder 
  ( FindResult(..) )
import GHC.Unit.Module
  ( mkModuleName )
import GHC.Unit.Module.Name
  ( ModuleName )
import GHC.Unit.Types
  ( Module )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO ( liftIO ) )

-- ghc-tcplugin-api
import GHC.TcPlugin.API.Internal

--------------------------------------------------------------------------------

-- | A type-checking plugin stage that supports monadic operations.
-- 
-- The following formulations can be used interchangeably:
-- 
-- > MonadTcPlugin m => m a
-- 
-- > TcPluginMonadStage s => TcPluginM s a
type     TcPluginMonadStage :: TcPluginStage -> Constraint
class    ( MonadTcPlugin ( TcPluginM stage ) ) => TcPluginMonadStage stage
instance ( MonadTcPlugin ( TcPluginM stage ) ) => TcPluginMonadStage stage

-- | A type-checking plugin stage that supports throwing type errors.
-- 
-- The following formulations can be used interchangeably:
-- 
-- > MonadTcPluginTypeError m => m a
-- 
-- > TcPluginMonadTypeErrorStage s => TcPluginM s a
type     TcPluginTypeErrorStage :: TcPluginStage -> Constraint
class    ( MonadTcPluginTypeError ( TcPluginM stage ) ) => TcPluginTypeErrorStage stage
instance ( MonadTcPluginTypeError ( TcPluginM stage ) ) => TcPluginTypeErrorStage stage

--------------------------------------------------------------------------------

tcPluginIO :: MonadTcPlugin m => IO a -> m a
tcPluginIO = unsafeLiftTcM . liftIO

tcPluginTrace :: MonadTcPlugin m => String -> SDoc -> m ()
tcPluginTrace a b = unsafeLiftTcM $ GHC.traceTc a b

--------------------------------------------------------------------------------

findImportedModule :: MonadTcPlugin m
                   => ModuleName -> Maybe FastString -> m FindResult
findImportedModule mod_name mb_pkg = liftTcPluginM $ GHC.findImportedModule mod_name mb_pkg

lookupOrig :: MonadTcPlugin m => Module -> OccName -> m Name
lookupOrig md = liftTcPluginM . GHC.lookupOrig md

tcLookupGlobal :: MonadTcPlugin m => Name -> m TyThing
tcLookupGlobal = liftTcPluginM . GHC.tcLookupGlobal

tcLookupTyCon :: MonadTcPlugin m => Name -> m TyCon
tcLookupTyCon = liftTcPluginM . GHC.tcLookupTyCon

tcLookupDataCon :: MonadTcPlugin m => Name -> m DataCon
tcLookupDataCon = liftTcPluginM . GHC.tcLookupDataCon

tcLookupClass :: MonadTcPlugin m => Name -> m Class
tcLookupClass = liftTcPluginM . GHC.tcLookupClass

tcLookup :: MonadTcPlugin m => Name -> m TcTyThing
tcLookup = liftTcPluginM . GHC.tcLookup

tcLookupId :: MonadTcPlugin m => Name -> m Id
tcLookupId = liftTcPluginM . GHC.tcLookupId

--------------------------------------------------------------------------------

getTopEnv :: MonadTcPlugin m => m HscEnv
getTopEnv = liftTcPluginM GHC.getTopEnv

getEnvs :: MonadTcPlugin m => m ( TcGblEnv, TcLclEnv )
getEnvs = liftTcPluginM GHC.getEnvs

getInstEnvs :: MonadTcPlugin m => m InstEnvs
getInstEnvs = liftTcPluginM GHC.getInstEnvs

getFamInstEnvs :: MonadTcPlugin m => m ( FamInstEnv, FamInstEnv )
getFamInstEnvs = liftTcPluginM GHC.getFamInstEnvs

matchFam :: MonadTcPlugin m
         => TyCon -> [ TcType ]
         -> m ( Maybe Reduction )
matchFam tycon args = liftTcPluginM $ GHC.matchFam tycon args

--------------------------------------------------------------------------------

newUnique :: MonadTcPlugin m => m Unique
newUnique = liftTcPluginM GHC.newUnique

newFlexiTyVar :: MonadTcPlugin m => Kind -> m TcTyVar
newFlexiTyVar = liftTcPluginM . GHC.newFlexiTyVar

isTouchableTcPluginM :: MonadTcPlugin m => TcTyVar -> m Bool
isTouchableTcPluginM = liftTcPluginM . GHC.isTouchableTcPluginM

--------------------------------------------------------------------------------

zonkTcType :: MonadTcPlugin m => TcType -> m TcType
zonkTcType = liftTcPluginM . GHC.zonkTcType

zonkCt :: MonadTcPlugin m => Ct -> m Ct
zonkCt = liftTcPluginM . GHC.zonkCt

--------------------------------------------------------------------------------

newWanted :: MonadTcPlugin m => CtLoc -> PredType -> m CtEvidence
newWanted loc pty = liftTcPluginM $ GHC.newWanted loc pty

-- | Create a new derived constraint.
newDerived :: MonadTcPlugin m => CtLoc -> PredType -> m CtEvidence
newDerived loc pty = liftTcPluginM $ GHC.newDerived loc pty

-- | Create a new given constraint, with the supplied evidence.
-- 
-- Use 'setCtLocM' to pass along the location information,
-- as only the 'CtOrigin' gets taken into account here.
newGiven :: CtLoc -> PredType -> EvExpr -> TcPluginM Solve CtEvidence
newGiven loc pty evtm = do
  tc_evbinds <- askEvBinds
  liftTcPluginM $ GHC.newGiven tc_evbinds loc pty evtm

setCtLocM :: MonadTcPlugin m => CtLoc -> m a -> m a
setCtLocM loc = unsafeLiftThroughTcM ( GHC.setCtLocM loc )

-- | Use the 'RewriteEnv' to set the 'CtLoc' for a computation.
setCtLocRewriteM :: TcPluginM Rewrite a -> TcPluginM Rewrite a
setCtLocRewriteM ma = do
  rewriteCtLoc <- fe_loc <$> askRewriteEnv
  setCtLocM rewriteCtLoc ma

--------------------------------------------------------------------------------

-- | Create a fresh evidence variable.
newEvVar :: PredType -> TcPluginM Solve EvVar
newEvVar = liftTcPluginM . GHC.newEvVar

-- | Create a fresh coercion hole.
newCoercionHole :: PredType -> TcPluginM Solve CoercionHole
newCoercionHole = liftTcPluginM . GHC.newCoercionHole

-- | Bind an evidence variable.
setEvBind :: EvBind -> TcPluginM Solve ()
setEvBind ev_bind = do
  tc_evbinds <- askEvBinds
  liftTcPluginM $ GHC.setEvBind tc_evbinds ev_bind

--------------------------------------------------------------------------------

-- | Conjure up a coercion witnessing an equality between two types
-- at the given 'Role' ('Nominal' or 'Representational').
mkPluginUnivCo
  :: String -- ^ Name of equality (for debugging)
  -> Role
  -> TcType -- ^ LHS
  -> TcType -- ^ RHS
  -> Coercion
mkPluginUnivCo str role lhs rhs = mkUnivCo ( PluginProv str ) role lhs rhs

-- | Conjure up an evidence term for an equality between two types
-- at the given 'Role' ('Nominal' or 'Representational').
-- 
-- Use this to supply a proof of a wanted equality in 'TcPluginOK'.
mkPluginUnivEvTerm
  :: String -- ^ Name of equality (for debugging)
  -> Role
  -> TcType -- ^ LHS
  -> TcType -- ^ RHS
  -> EvTerm
mkPluginUnivEvTerm str role lhs rhs = evCoercion $ mkPluginUnivCo str role lhs rhs

-- | Provide a rewriting of a saturated type family application
-- at the given 'Role' ('Nominal' or 'Representational').
--
-- The result can be passed to 'TcPluginRewriteTo' to specify the outcome
-- of rewriting a type family application.
mkTyFamAppReduction
  :: String   -- ^ Name of reduction (for debugging)
  -> Role     -- ^ Role of reduction ('Nominal' or 'Representational')
  -> TyCon    -- ^ Type family 'TyCon'
  -> [TcType] -- ^ Type family arguments
  -> TcType   -- ^ The type that the type family application reduces to
  -> Reduction
mkTyFamAppReduction str role tc args ty =
  Reduction ty ( mkPluginUnivCo str role ( mkTyConApp tc args ) ty )
