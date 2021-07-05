{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: GHC.TcPlugin.API

This module provides a unified interface for writing type-checking plugins for GHC.

It attempts to re-export all the functionality from GHC that is relevant to plugin authors,
as well as providing utility functions to streamline certain common operations such as
creating evidence (to solve constraints), rewriting type family applications, throwing custom type errors.

Consider making use of the table of contents to help navigate this documentation;
don't hesitate to jump between sections to get an overview of the relevant aspects.

For a basic illustration of the functionality, check the examples in the associated
<https://github.com/sheaf/ghc-tcplugin-api GitHub repository>.

The internal module "GHC.TcPlugin.API.Internal" can be used to directly
lift and unlift computations in GHC's 'GHC.Tc.TcM' monad, but it is hoped that
the interface provided in this module is sufficient.

-}

module GHC.TcPlugin.API
  ( -- * Basic TcPlugin functionality

    -- | Use 'mkTcPlugin' to pass a type-checking plugin to GHC,
    -- embedded as a field of GHC's 'GHC.Plugins.Plugin' record.
    -- 
    -- Example for a pure plugin:
    -- 
    -- > import qualified GHC.Plugins as GHC
    -- >   ( Plugin(..), defaultPlugin, purePlugin )
    -- >
    -- > plugin :: GHC.Plugin
    -- > plugin =
    -- >   GHC.defaultPlugin
    -- >     { GHC.tcPlugin        = \ _args -> Just $ mkTcPlugin myTcPlugin
    -- >     , GHC.pluginRecompile = GHC.purePlugin
    -- >     }
    mkTcPlugin, tcPluginIO

    -- ** Plugin state
    -- | You will likely want to create a record type to hold information looked up
    -- by the plugin, such as the name of classes or type families your plugin will consider.
    -- For example:
    --
    -- > data MyDefinitions { myTyFam :: !TyCon, myClass :: !Class }
    --
    -- The 'tcPluginInit' part of the plugin looks up all this information and returns it:
    --
    -- > myTcPluginInit :: TcPluginM Init MyDefinitions
    --
    -- This step should also be used to initialise any external tools,
    -- such as an external SMT solver.
    --
    -- This datatype will then be passed to other stages of the plugin:
    --
    -- > myTcPluginSolve :: MyDefinitions -> TcPluginSolver

    -- ** The 'TcPlugin' type
  , TcPlugin(..), TcPluginStage(..)
  , TcPluginSolver
#if HAS_REWRITING
  , TcPluginSolveResult(..)
#else
  , TcPluginSolveResult
  , pattern TcPluginContradiction, pattern TcPluginOk
#endif
  , TcPluginRewriter, TcPluginRewriteResult(..)

    -- ** The type-checking plugin monads

    -- | Different stages of type-checking plugins have access to different information.
    -- For a unified interface, an MTL-style approach is used, with the 'MonadTcPlugin'
    -- typeclass providing overloading (for operations that work in all stages).
  , TcPluginM
  , MonadTcPlugin
  
    -- *** Emitting new work, and throwing type-errors
  , MonadTcPluginWork
  , TcPluginErrorMessage(..)
  , mkTcPluginErrorTy

    -- * Name resolution
    
    -- ** Packages and modules 

    -- | Use these functions to lookup a module,
    -- from the current package or imported packages.
  , findImportedModule
  , Module, ModuleName, FindResult(..)
  , FastString, fsLit, mkModuleName

    -- ** Names
 
    -- *** Occurence names

    -- | The most basic type of name is the 'OccName', which is a
    -- simple textual name within a namespace (e.g. the class namespace),
    -- without any disambiguation (no module qualifier, etc).
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc

    -- *** Names

    -- | After having looked up the 'Module', we can obtain the full 'Name'
    -- referred to by an 'OccName'. This is fully unambiguous, as it
    -- contains a 'Unique' identifier for the name.
  , lookupOrig

    -- *** 'TyCon', 'Class', 'DataCon', etc

    -- | Finally, we can obtain the actual objects we're interested in handling,
    -- such as classes, type families, data constructors... by looking them up
    -- using their 'Name'.
  , tcLookupTyCon
  , tcLookupDataCon
  , tcLookupClass
  , tcLookupGlobal
  , tcLookup
  , tcLookupId
  , promoteDataCon

    -- * Constraints

    -- | Type-checking plugins will often want to manipulate constraints,
    -- e.g. solve constraints that GHC can't solve on its own, or emit
    -- their own constraints.
    --
    -- There are two main types of constraints:
    --
    --   - Given constraints, which
    --     are already known and have evidence associated to them,
    --   - Wanted constraints, for which evidence has not yet been found.
    --
    -- When GHC can't solve a Wanted constraint, it will get reported to the
    -- user as a type error.

    -- | To get started, it can be helpful to immediately print out all the constraints
    -- that the plugin is given, using 'tcPluginTrace':
    -- 
    -- > solver _ givens deriveds wanteds = do
    -- >   tcPluginTrace "---Plugin start---" (ppr givens $$ ppr deriveds $$ ppr wanteds)
    -- >   pure $ TcPluginOk [] []
    --
    -- This creates a plugin that prints outs the constraints it is passed,
    -- without doing anything with them.
    --
    -- Note that pretty-printing in GHC is done using the 'Outputable' type class.
    -- We use its 'ppr' method to turn things into pretty-printable documents,
    -- and '($$)' to combine documents vertically.
    -- If you need more capabilities for pretty-printing documents,
    -- import GHC's "GHC.Utils.Outputable" module.
  , tcPluginTrace

    -- | To solve a constraint, one needs to provide evidence:
    --
    --   - a 'Coercion' witnessing an equality between two types,
    --     in order to solve an equality constraint,
    --   - an evidence term 'EvExpr', e.g. a dictionary of the methods
    --     for a typeclass constraint.
  , newWanted, newGiven

    -- | Derived constraints are like Wanted constraints, except that they
    -- don't require evidence in order to be solved, and won't be seen
    -- in error messages if they go unsolved.
    --
    -- Solver plugins usually ignore this type of constraint entirely.
    -- They occur mostly when dealing with functional dependencies and type-family
    -- injectivity annotations.
    --
    -- GHC 9.4 removes this flavour of constraints entirely, subsuming their uses into
    -- Wanted constraints.
  , askDeriveds, newDerived
    
    -- ** Predicates
    
    -- | A predicate is the underlying representation of a constraint,
    -- for instance a record containing the typeclass methods for a typeclass constraint.

    -- | The following functions allow plugins to create constraints
    -- for typeclasses and type equalities.
  , mkClassPred, mkPrimEqPredRole

    -- ** Location information and 'CtLoc's

    -- | When creating new constraints, one still needs a mechanism allowing GHC
    -- to report a certain source location associated to them when throwing an error,
    -- as well as other information the type-checker was aware of at that point
    -- (e.g. available instances, given constraints, etc).
    --
    -- This is the purpose of 'CtLoc'.
  , setCtLocM
  , setCtLocRewriteM

    -- | 'bumpCtLocDepth' adds one to the "depth" of the constraint.
    -- Can help avoid loops, by triggering a "maximum depth exceeded" error.
  , bumpCtLocDepth

    -- ** Canonical and non-canonical constraints

    -- | A constraint in GHC starts out as "non-canonical", which means that
    -- GHC doesn't know what type of constraint it is.
    -- GHC will inspect the constraint to turn it into a canonical form
    -- (class constraint, equality constraint, etc.) which satisfies certain
    -- invariants used during constraint solving.
    --
    -- Thus, whenever emitting new constraints, it is usually best to emit a
    -- non-canonical constraint, letting GHC canonicalise it.
  , mkNonCanonical

    -- ** Analysing types, constraints & predicates

    -- | A type-checking plugin might need to inspect constraints itself
    -- to figure out what it is dealing with.
    -- 
    -- In general, type-checking plugins can encounter all sorts of constraints,
    -- whether in canonical form or not.
    -- In order to handle these constraints in a uniform manner, it is usually
    -- preferable to inspect each constraint's predicate, which can be obtained
    -- by using `ctPred`'classifyPredType'.
  , classifyPredType, ctPred
  , eqType
  , ctLoc, ctEvidence, ctFlavour, ctEqRel, ctOrigin
  , getInstEnvs

    -- ** Constraint evidence
  
    -- *** Coercions

    -- | Coercions are the evidence for type equalities.
    -- As such, when proving an equality, a type-checker plugin needs
    -- to construct the associated coercions.
  , mkPluginUnivCo
  , newCoercionHole
  , mkUnivCo

    -- *** Evidence terms

    -- | Evidence terms are a slightly more general notion than 'Coercion's,
    -- as they can also be used as evidence for typeclass constraints.
    --
    -- This means that a plugin that wants to solve a class constraint
    -- will need to provide an evidence term, making use of the evidence
    -- that is already available.

  , mkPluginUnivEvTerm
  , newEvVar, setEvBind
  , evCoercion
--, askEvBinds

    -- *** Class dictionaries

    -- | To create evidence terms for class constraints, type-checking plugins
    -- need to be able to construct the appropriate dictionaries containing
    -- the values for the class methods.
    --
    -- The class dictionary constructor can be obtained using 'classDataCon'.
    -- Functions from "GHC.Core.Make", re-exported here, will be useful for
    -- constructing the necessary terms, e.g. 'mkCoreApp' for an application.

  , classDataCon
  , module GHC.Core.Make

    -- * Type family applications

    -- ** Querying for type family reductions

  , matchFam
  , getFamInstEnvs
  , FamInstEnv

    -- ** Specifying type family reductions

    -- | A plugin that wants to rewrite a type family application must provide two
    -- pieces of information:
    --
    --   - the type that the type family application reduces to,
    --   - evidence for this reduction, i.e. a 'Coercion' proving the equality.
    --
    -- In the rewriting stage, type-checking plugins have access to the rewriter
    -- environment 'RewriteEnv', which has information about the location of the
    -- type family application, the local type-checking environment, among other things.
    --
    -- Note that a plugin should provide a 'UniqFM' from 'TyCon' to rewriting functions,
    -- which specifies a rewriting function for each type family.
    -- Use 'emptyUFM' or 'listToUFM' to construct this map,
    -- or import the GHC module "GHC.Types.Unique.FM" for a more complete API.
  , askRewriteEnv, rewriteEnvCtLoc, RewriteEnv
  , mkTyFamAppReduction, Reduction(..)

    -- * Handling Haskell types

    -- ** Type variables
  , newUnique
  , newFlexiTyVar
  , isTouchableTcPluginM
  , mkTyVarTy, mkTyVarTys
  , getTyVar_maybe
  , TcType, TcTyVar, Unique, Kind

    -- ** Creating and decomposing applications
  , mkTyConTy, mkTyConApp, splitTyConApp_maybe
  , mkAppTy, mkAppTys

    -- ** Function types

  , AnonArgFlag(..), Mult
  , mkFunTy, mkVisFunTy, mkInvisFunTy, mkVisFunTys
  , mkForAllTy, mkForAllTys, mkInvisForAllTys
  , mkPiTy, mkPiTys
  , mkFunTyMany
  , mkScaledFunTy
  , mkVisFunTyMany, mkVisFunTysMany
  , mkInvisFunTyMany, mkInvisFunTysMany

    -- ** Zonking

    -- | Zonking is the operation in which GHC actually switches out mutable unification variables
    -- for their actual filled in type.
    --
    -- See the Note [What is zonking?] in GHC's source code for more information.
  , zonkTcType
  , zonkCt

    -- ** Map-like data structures based on 'Unique's
    
    -- | Import "GHC.Types.Unique.FM" or "GHC.Types.Unique.DFM" for
    -- a more complete interface to maps whose keys are 'Unique's.

  , UniqDFM
  , lookupUDFM, lookupUDFM_Directly, elemUDFM
  , UniqFM
  , emptyUFM, listToUFM

    -- * The type-checking environment
  , getEnvs

    -- * Built-in types

    -- | This module also re-exports the built-in types that GHC already knows about.
    --
    -- This allows plugins to directly refer to e.g. the promoted data constructor
    -- 'True' without having to look up its name.
  , module GHC.Builtin.Types

    -- * GHC types

    -- | These are the types that the plugin will inspect and manipulate.

    -- | = END OF API DOCUMENTATION

    -- | == Names
  , Name, OccName, TyThing, TcTyThing
  , Class(classTyCon), DataCon, TyCon, Id

    -- | == Constraints
  , Pred(..), EqRel(..), FunDep, CtFlavour
  , Ct, CtLoc, CtEvidence, CtOrigin
  , QCInst
#if MIN_VERSION_ghc(9,2,0)
  , CanEqLHS
#endif
  , Type, PredType
  , InstEnvs, TcLevel

    -- | === Coercions and evidence
  , Coercion, Role(..), UnivCoProvenance
  , CoercionHole
  , EvBind, EvTerm, EvVar, EvExpr, EvBindsVar

    -- | == The type-checking environment
  , TcGblEnv, TcLclEnv

    -- | == Pretty-printing
  , SDoc, Outputable(..)

  )
  where

-- ghc
import GHC
  ( TyThing(..) )
import GHC.Builtin.Types
import GHC.Core.Class
  ( Class(..), FunDep )
import GHC.Core.Coercion
  ( mkUnivCo, mkPrimEqPredRole
#if HAS_REWRITING
  , Reduction(..)
#endif
  )
import GHC.Core.Coercion.Axiom
  ( Role(..) )
import GHC.Core.DataCon
  ( DataCon
  , classDataCon, promoteDataCon
  )
import GHC.Core.FamInstEnv
  ( FamInstEnv )
import GHC.Core.InstEnv
  ( InstEnvs(..) )
import GHC.Core.Make
import GHC.Core.Predicate
  ( Pred(..), EqRel(..)
  , classifyPredType, mkClassPred
  )
import GHC.Core.TyCon
  ( TyCon(..) )
import GHC.Core.TyCo.Rep
  ( Type, PredType, Kind
  , Coercion(..), CoercionHole(..)
  , UnivCoProvenance(..)
  , AnonArgFlag(..), Mult
  , mkTyVarTy, mkTyVarTys
  , mkFunTy, mkVisFunTy, mkInvisFunTy, mkVisFunTys
  , mkForAllTy, mkForAllTys, mkInvisForAllTys
  , mkPiTy, mkPiTys
  , mkFunTyMany
  , mkScaledFunTy
  , mkVisFunTyMany, mkVisFunTysMany
  , mkInvisFunTyMany, mkInvisFunTysMany
  )
import GHC.Core.Type
  ( eqType, mkTyConTy, mkTyConApp, splitTyConApp_maybe
  , mkAppTy, mkAppTys, getTyVar_maybe
  )
import GHC.Data.FastString
  ( FastString, fsLit )
import qualified GHC.Tc.Plugin
  as GHC
import GHC.Tc.Types
  ( TcTyThing(..), TcGblEnv(..), TcLclEnv(..)
#if HAS_REWRITING
  , TcPluginSolveResult(..), TcPluginRewriteResult(..)
  , RewriteEnv(..)
#else
  , TcPluginResult(..)
#endif
  )
import GHC.Tc.Types.Constraint
  ( Ct(..), CtLoc(..), CtEvidence(..), CtFlavour(..)
  , QCInst(..)
#if MIN_VERSION_ghc(9,2,0)
  , CanEqLHS(..)
#endif
  , ctPred, ctLoc, ctEvidence
  , ctFlavour, ctEqRel, ctOrigin
  , bumpCtLocDepth
  , mkNonCanonical
  )
import GHC.Tc.Types.Evidence
  ( EvBind(..), EvTerm(..), EvExpr, EvBindsVar(..)
  , evCoercion
  )
import GHC.Tc.Types.Origin
  ( CtOrigin(..) )
import qualified GHC.Tc.Utils.Monad
  as GHC
    ( traceTc, setCtLocM )
import GHC.Tc.Utils.TcType
  ( TcType, TcLevel )
import GHC.Types.Name
  ( Name )
import GHC.Types.Name.Occurrence
  ( OccName(..)
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc
  )
import GHC.Types.Unique
  ( Unique )
import GHC.Types.Unique.FM
  ( UniqFM, emptyUFM, listToUFM )
import GHC.Types.Unique.DFM
  ( UniqDFM, lookupUDFM, lookupUDFM_Directly, elemUDFM )
import GHC.Types.Var
  ( Id, TcTyVar, EvVar )
import GHC.Utils.Outputable
  ( Outputable(..), SDoc )
#if MIN_VERSION_ghc(9,2,0)
import GHC.Unit.Finder
  ( FindResult(..) )
#else
import GHC.Driver.Finder
  ( FindResult(..) )
#endif
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
#ifndef HAS_REWRITING
import GHC.TcPlugin.API.Internal.Shim
#endif

--------------------------------------------------------------------------------

-- | Run an 'IO' computation within the plugin.
tcPluginIO :: MonadTcPlugin m => IO a -> m a
tcPluginIO = unsafeLiftTcM . liftIO

-- | Output some debugging information within the plugin.
tcPluginTrace :: MonadTcPlugin m
              => String -- ^ Text at the top of the debug message.
              -> SDoc   -- ^ Formatted document to print (use the 'ppr' pretty-printing function to obtain an 'SDoc' from any 'Outputable')
              -> m ()
tcPluginTrace a b = unsafeLiftTcM $ GHC.traceTc a b

--------------------------------------------------------------------------------

-- | Lookup a Haskell module from the given package.
findImportedModule :: MonadTcPlugin m
                   => ModuleName -- ^ Module name, e.g. @"Data.List"@.
                   -> Maybe FastString -- ^ Package name, e.g. @Just "base"@.
                                       -- Use @Nothing@ for the current home package
                   -> m FindResult
findImportedModule mod_name mb_pkg = liftTcPluginM $ GHC.findImportedModule mod_name mb_pkg

-- | Obtain the full internal 'Name' (with its unique identifier, etc) from its 'OccName'.
--
-- Example usage:
--
-- > lookupOrig preludeModule ( mkTcOcc "Bool" )
--
-- This will obtain the 'Name' associated with the type 'Bool'.
--
-- You can then call 'tcLookupTyCon' to obtain the associated 'TyCon'.
lookupOrig :: MonadTcPlugin m => Module -> OccName -> m Name
lookupOrig md = liftTcPluginM . GHC.lookupOrig md

-- | Lookup a type constructor from its name (datatype, type synonym or type family).
tcLookupTyCon :: MonadTcPlugin m => Name -> m TyCon
tcLookupTyCon = liftTcPluginM . GHC.tcLookupTyCon

-- | Lookup a data constructor (such as 'True', 'Just', ...) from its name.
tcLookupDataCon :: MonadTcPlugin m => Name -> m DataCon
tcLookupDataCon = liftTcPluginM . GHC.tcLookupDataCon

-- | Lookup a typeclass from its name.
tcLookupClass :: MonadTcPlugin m => Name -> m Class
tcLookupClass = liftTcPluginM . GHC.tcLookupClass

-- | Lookup a global typecheckable-thing from its name.
tcLookupGlobal :: MonadTcPlugin m => Name -> m TyThing
tcLookupGlobal = liftTcPluginM . GHC.tcLookupGlobal

-- | Lookup a typecheckable-thing available in a local context,
-- such as a local type variable.
tcLookup :: MonadTcPlugin m => Name -> m TcTyThing
tcLookup = liftTcPluginM . GHC.tcLookup

-- | Lookup an identifier, such as a type variable.
tcLookupId :: MonadTcPlugin m => Name -> m Id
tcLookupId = liftTcPluginM . GHC.tcLookupId

--------------------------------------------------------------------------------

{-
getTopEnv :: MonadTcPlugin m => m HscEnv
getTopEnv = liftTcPluginM GHC.getTopEnv
-}

-- | Obtain the current global and local type-checking environments.
getEnvs :: MonadTcPlugin m => m ( TcGblEnv, TcLclEnv )
getEnvs = liftTcPluginM GHC.getEnvs

-- | Obtain all currently-reachable typeclass instances.
getInstEnvs :: MonadTcPlugin m => m InstEnvs
getInstEnvs = liftTcPluginM GHC.getInstEnvs

-- | Obtain all currently-reachable data/type family instances.
--
-- First result: external instances.
-- Second result: instances in the current home package.
getFamInstEnvs :: MonadTcPlugin m => m ( FamInstEnv, FamInstEnv )
getFamInstEnvs = liftTcPluginM GHC.getFamInstEnvs

-- | Ask GHC what a type family application reduces to.
--
-- __Warning__: can cause a loop when used within 'tcPluginRewrite'.
matchFam :: MonadTcPlugin m
         => TyCon -> [ TcType ]
         -> m ( Maybe Reduction )
matchFam tycon args =
#ifndef HAS_REWRITING
  fmap mkReduction <$>
#endif
  ( liftTcPluginM $ GHC.matchFam tycon args )

--------------------------------------------------------------------------------

-- | Create a new unique. Useful for generating new variables in the plugin.
newUnique :: MonadTcPlugin m => m Unique
newUnique = liftTcPluginM GHC.newUnique

-- | Create a new meta-variable (unification variable) of the given kind. 
newFlexiTyVar :: MonadTcPlugin m => Kind -> m TcTyVar
newFlexiTyVar = liftTcPluginM . GHC.newFlexiTyVar

-- | Query whether a type variable is touchable:
--   - is it a unification variable (and not a skolem variable)?
--   - is it actually unifiable given the current 'TcLevel'?
isTouchableTcPluginM :: MonadTcPlugin m => TcTyVar -> m Bool
isTouchableTcPluginM = liftTcPluginM . GHC.isTouchableTcPluginM

--------------------------------------------------------------------------------

-- | Zonk the given type, which takes the metavariables in the type and
-- substitutes their actual value.
zonkTcType :: MonadTcPlugin m => TcType -> m TcType
zonkTcType = liftTcPluginM . GHC.zonkTcType

-- | Zonk a given constraint.
zonkCt :: MonadTcPlugin m => Ct -> m Ct
zonkCt = liftTcPluginM . GHC.zonkCt

--------------------------------------------------------------------------------

-- | Create a new derived constraint.
--
-- Requires a location (so that error messages can say where the constraint came from,
-- what things were in scope at that point, etc), as well as the actual constraint (encoded as a type).
newWanted :: MonadTcPluginWork m => CtLoc -> PredType -> m CtEvidence
newWanted loc pty = liftTcPluginM $ GHC.newWanted loc pty

-- | Create a new derived constraint. See also 'newWanted'.
newDerived :: MonadTcPluginWork m => CtLoc -> PredType -> m CtEvidence
newDerived loc pty = liftTcPluginM $ GHC.newDerived loc pty

-- | Create a new given constraint.
-- 
-- Unlike 'newWanted' and 'newDerived', we need to supply evidence
-- for this constraint.
-- 
-- Use 'setCtLocM' to pass along the location information,
-- as only the 'CtOrigin' gets taken into account here.
newGiven :: CtLoc -> PredType -> EvExpr -> TcPluginM Solve CtEvidence
newGiven loc pty evtm = do
#if HAS_REWRITING
  tc_evbinds <- askEvBinds
  liftTcPluginM $ GHC.newGiven tc_evbinds loc pty evtm
#else
  liftTcPluginM $ GHC.newGiven loc pty evtm
#endif


-- | Obtain the 'CtLoc' from a 'RewriteEnv'.
--
-- This can be useful to obtain the location of the
-- constraint currently being rewritten,
-- so that newly emitted constraints can be given
-- the same location information.
rewriteEnvCtLoc :: RewriteEnv -> CtLoc
rewriteEnvCtLoc = fe_loc

-- | Set the location information for a computation,
-- so that the constraint solver reports an error at the given location.
setCtLocM :: MonadTcPluginWork m => CtLoc -> m a -> m a
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
#if HAS_REWRITING
  tc_evbinds <- askEvBinds
  liftTcPluginM $ GHC.setEvBind tc_evbinds ev_bind
#else
  liftTcPluginM $ GHC.setEvBind ev_bind
#endif

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
-- Use this to supply a proof of a wanted equality in 'TcPluginOk'.
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
  Reduction ty ( mkPluginUnivCo str role ty ( mkTyConApp tc args ) )
