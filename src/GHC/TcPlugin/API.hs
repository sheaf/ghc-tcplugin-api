{-# LANGUAGE CPP #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

#if !MIN_VERSION_ghc(8,11,0)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module: GHC.TcPlugin.API

This module provides a unified interface for writing type-checking plugins for GHC.

It attempts to re-export all the functionality from GHC that is relevant to plugin authors,
as well as providing utility functions to streamline certain common operations such as
creating evidence (to solve constraints), rewriting type family applications, throwing custom type errors.

Consider making use of the table of contents to help navigate this documentation;
don't hesitate to jump between sections to get an overview of the relevant aspects.

For an illustration of the functionality, check the examples in the associated
<https://github.com/sheaf/ghc-tcplugin-api GitHub repository>.

The internal module "GHC.TcPlugin.API.Internal" can be used to directly
lift and unlift computations in GHC's 'GHC.Tc.Types.TcM' monad, but it is hoped that
the interface provided in this module is sufficient.

-}

module GHC.TcPlugin.API
  ( -- * Basic TcPlugin functionality

    -- ** The 'TcPlugin' type
    TcPlugin(..)
  , mkTcPlugin

    -- ** Plugin state
    -- | A type-checker plugin can define its own state, corresponding to the existential parameter @s@
    -- in the definition of 'TcPlugin'.
    -- This allows a plugin to look up information a single time
    -- on initialisation, and pass it on for access in all further invocations of the plugin.
    --
    -- For example:
    --
    -- > data MyDefinitions { myTyFam :: !TyCon, myClass :: !Class }
    --
    -- Usually, the 'tcPluginInit' part of the plugin looks up all this information and returns it:
    --
    -- > myTcPluginInit :: TcPluginM Init MyDefinitions
    --
    -- This step should also be used to initialise any external tools,
    -- such as an external SMT solver.
    --
    -- This information will then be passed to other stages of the plugin:
    --
    -- > myTcPluginSolve :: MyDefinitions -> TcPluginSolver

    -- ** The type-checking plugin monads

    -- | Different stages of type-checking plugins have access to different information.
    -- For a unified interface, an MTL-style approach is used, with the 'MonadTcPlugin'
    -- typeclass providing overloading (for operations that work in all stages).
  , TcPluginStage(..), MonadTcPlugin
  , TcPluginM

    -- *** Emitting new work, and throwing type-errors

    -- | Some operations only make sense in the two main phases, solving and rewriting.
    -- This is captured by the 'MonadTcPluginWork' typeclass, which allows emitting
    -- new work, including throwing type errors.
  , MonadTcPluginWork
  , TcPluginErrorMessage(..)
  , mkTcPluginErrorTy

    -- * Name resolution

    -- | Name resolution is usually the first step in writing a type-checking plugin:
    -- plugins need to look up the names of the objects they want to manipulate.
    --
    -- The most convenient way to look up a Name is to bring the name into scope
    -- in the module defining the plugin, and use @TemplateHaskellQuotes@
    -- in conjunction with the 'lookupTHName' function.
    --
    -- > import Data.Maybe
    -- >
    -- > lookupMaybeTyCon :: MonadTcPluginM m => m TyCon
    -- > lookupMaybeTyCon = lookupTHName ''Maybe >>= tcLookupTyCon
    --
    -- This is recommended over manually looking up the package and module
    -- name, because the manual method does not robustly deal with re-exports;
    -- one must specify the exact package/module which defines the identifier.
    -- For instance, to manually lookup the type family @MyFam@ in module
    -- @MyModule@ in package @my-pkg@:
    --
    -- > lookupMyFam :: MonadTcPluginM m => m Name
    -- > lookupMyFam = do
    -- >    let modlName = mkModuleName "MyModule"
    -- >        pkgName  = Just $ fsLit "my-pkg"
    -- >    pkgQual    <- resolveImport      modlName pkgName
    -- >    findResult <- findImportedModule modlName pkgQual
    -- >    case findResult of
    -- >      Found _ myModule -> pure myModule
    -- >      _ -> error "MyPlugin couldn't find MyModule in my-pkg"
    -- >
    -- > lookupMyFam :: MonadTcPlugin m => Module -> m TyCon
    -- > lookupMyFam myModule = tcLookupTyCon =<< lookupOrig myModule ( mkTcOcc "MyFam" )
    --
    -- Most of these operations should be performed in 'tcPluginInit', and passed on
    -- to the other stages: the plugin initialisation is called only once in each module
    -- that the plugin is used, whereas the solver and rewriter are usually called repeatedly.

    -- ** Name resolution: TH method

    -- | Look up a Name from a Template Haskell Name (e.g. a Name constructed
    -- using TemplateHaskellQuotes).
  , lookupTHName

    -- ** Name resolution: manual method
    -- *** Packages and modules

    -- | Use these functions to lookup a module,
    -- from the current package or imported packages.
  , findImportedModule, resolveImport, fsLit, unpackFS, mkModuleName
  , unitIdFS, stringToUnitId, pkgQualToPkgName
  , Module, ModuleName, FindResult(..), UnitId, PkgQual

    -- *** Names

    -- **** Occurence names

    -- | The most basic type of name is the 'OccName', which is a
    -- simple textual name within a namespace (e.g. the class namespace),
    -- without any disambiguation (no module qualifier, etc).
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc

    -- **** Names

    -- | After having looked up the 'Module', we can obtain the full 'Name'
    -- referred to by an 'OccName'. This is fully unambiguous, as it
    -- contains a 'Unique' identifier for the name.
  , lookupOrig

    -- ** Name resolution: resolving the Name to a 'TyCon', 'Class', 'DataCon', etc

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

    -- * Constraint solving

    -- | Type-checking plugins will often want to manipulate constraints,
    -- e.g. solve constraints that GHC can't solve on its own, or emit
    -- their own constraints.
    --
    -- There are two different constraint flavours:
    --
    --   - Given constraints, which are already known and
    --     have evidence associated to them,
    --   - Wanted constraints, for which evidence has not yet been found.
    --
    -- When GHC can't solve a Wanted constraint, it will get reported to the
    -- user as a type error.

  , TcPluginSolver
#if HAS_REWRITING
  , TcPluginSolveResult(..)
#else
  , TcPluginSolveResult
  , pattern TcPluginContradiction, pattern TcPluginOk
#endif

    -- | The 'tcPluginSolve' method of a typechecker plugin will be invoked
    -- in two different ways:
    --
    -- 1. to simplify Given constraints. In this case, the 'tcPluginSolve' function
    --    will not be passed any Wanted constraints, and
    -- 2. to solve Wanted constraints.
    --
    -- The plugin can then respond in one of two ways:
    --
    --   - with @TcPluginOk solved new@, where @solved@ is a list of solved constraints
    --     and @new@ is a list of new constraints for GHC to process;
    --   - with @TcPluginContradiction contras@, where @contras@ is a list of impossible
    --     constraints, so that they can be turned into errors.
    --
    -- In both cases, the plugin must respond with constraints of the same flavour,
    -- i.e. in (1) it should return only Givens, and for (2) it should return only
    -- Wanteds; all other constraints will be ignored.

    -- ** Getting started with constraint solving

    -- | To get started, it can be helpful to immediately print out all the constraints
    -- that the plugin is given, using 'tcPluginTrace':
    --
    -- > solver _ givens wanteds = do
    -- >   tcPluginTrace "---Plugin start---" (ppr givens $$ ppr wanteds)
    -- >   pure $ TcPluginOk [] []
    --
    -- This creates a plugin that prints outs the constraints it is passed,
    -- without doing anything with them.
    --
    -- To see this output, you will need to pass the flags @-ddump-tc-trace@
    -- and @-ddump-to-file@ to GHC. This will output the trace as a log file,
    -- and you can search for @"---Plugin start---"@ to find the plugin inputs.
    --
    -- Note that pretty-printing in GHC is done using the 'Outputable' type class.
    -- We use its 'ppr' method to turn things into pretty-printable documents,
    -- and '($$)' to combine documents vertically.
    -- If you need more capabilities for pretty-printing documents,
    -- import GHC's "GHC.Utils.Outputable" module.
  , tcPluginTrace

    -- ** Inspecting constraints & predicates

    -- *** Canonical and non-canonical constraints

    -- | A constraint in GHC starts out as "non-canonical", which means that
    -- GHC doesn't know what type of constraint it is.
    -- GHC will inspect the constraint to turn it into a canonical form
    -- (class constraint, equality constraint, etc.) which satisfies certain
    -- invariants used during constraint solving.
    --
    -- Thus, whenever emitting new constraints, it is usually best to emit a
    -- non-canonical constraint, letting GHC canonicalise it.
  , mkNonCanonical

    -- *** Predicates

    -- | A type-checking plugin will usually need to inspect constraints,
    -- so that it can pick out the constraints it is going to interact with.
    --
    -- In general, type-checking plugins can encounter all sorts of constraints,
    -- whether in canonical form or not.
    -- In order to handle these constraints in a uniform manner, it is usually
    -- preferable to inspect each constraint's predicate, which can be obtained
    -- by using 'classifyPredType' and 'ctPred'.
    --
    -- This allows the plugin to determine what kind of constraints it is dealing with:
    --
    --   - an equality constraint? at 'Nominal' or 'Representational' role?
    --   - a type-class constraint? for which class?
    --   - an irreducible constraint, e.g. something of the form @c a@?
    --   - a quantified constraint?
  , Pred
  , pattern ClassPred, pattern EqPred, pattern IrredPred, pattern ForAllPred
  , classifyPredType, ctPred

    -- | == Handling type variables
  , TyVar, CoVar
  , MetaDetails, MetaInfo
  , isSkolemTyVar
  , isMetaTyVar, isFilledMetaTyVar_maybe
  , writeMetaTyVar
  , readTcRef, writeTcRef

    -- | == Some further functions for inspecting constraints
  , eqType, nonDetCmpType
  , ctLoc, ctEvidence, ctFlavour, ctEqRel, ctOrigin
  , ctEvPred, ctEvId, ctEvExpr, ctEvLoc
  , isGiven, isWanted
  , isEqPred, isEqClassPred
  , className, tyConName

    -- ** Constraint evidence

    -- *** Coercions

    -- | 'GHC.Core.TyCo.Rep.Coercion's are the evidence for type equalities.
    -- As such, when proving an equality, a type-checker plugin needs
    -- to construct the associated coercions.
  , mkPluginUnivCo
  , newCoercionHole
  , mkReflCo, mkSymCo, mkTransCo, mkUnivCo
  , mkCoercionTy, isCoercionTy, isCoercionTy_maybe

    -- *** Depending on outer Givens

    -- | When a plugin returns a coercion that depends on outer Given constraints,
    -- it should declare this dependency using the '[Coercion]' argument to
    -- functions such as 'mkPluginUnivCo', 'mkPluginUnivEvTerm' and 'mkTyFamAppReduction'
    -- in order to avoid this coercion getting floated out past such enclosing
    -- Givens.
    --
    -- You can use 'ctEvCoercion' to obtain the coercion underlying an equality
    -- constraint (whether Given or Wanted). It is not possible to declare
    -- a dependency on non-equality constraints, and calling 'ctEvCoercion'
    -- on a non-equality constraint will cause a crash.
  , ctEvCoercion

    -- *** Evidence terms

    -- | Typeclass constraints have a different notion of evidence: evidence terms.
    --
    -- A plugin that wants to solve a class constraint will need to provide
    -- an evidence term. Such evidence can be created from scratch, or it can be obtained
    -- by combining evidence that is already available.

  , mkPluginUnivEvTerm
  , evDFunApp, evDataConApp
  , newEvVar, setEvBind
  , evId, evCoercion, evCast
  , askEvBinds, lookupEvBind, eb_lhs, eb_rhs
  , newName, mkLocalId, mkTyVar

    -- *** Class dictionaries

    -- | To create evidence terms for class constraints, type-checking plugins
    -- need to be able to construct the appropriate dictionaries containing
    -- the values for the class methods.
    --
    -- The class dictionary constructor can be obtained using 'classDataCon'.
    -- Functions from "GHC.Core.Make", which is re-exported by this library,
    -- will be useful for constructing the necessary terms
    --
    -- For instance, we can apply the class data constructor using 'mkCoreConApps'.
    -- Remember that the type-level arguments (the typeclass variables) come first,
    -- before the actual evidence term (the class dictionary expression).

  , classDataCon
#if !MIN_VERSION_ghc(9,0,0)
  , mkUncheckedIntExpr
#endif

    -- | ==== Class instances

    -- | In some cases, a type-checking plugin might need to access the
    -- class instances that are currently in scope, e.g. to obtain certain
    -- evidence terms.
  , getInstEnvs

    -- ** Emitting new constraints

  , newWanted, newGiven

    -- | The following functions allow plugins to create constraints
    -- for typeclasses and type equalities.
  , mkClassPred, mkEqPredRole

    -- | === Deriveds

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
  , askDeriveds

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

    -- * Rewriting type-family applications

  , TcPluginRewriter, TcPluginRewriteResult(..)

    -- ** Querying for type family reductions

  , matchFam
  , getFamInstEnvs
  , FamInstEnv

    -- ** Specifying type family reductions

    -- | A plugin that wants to rewrite a type family application must provide two
    -- pieces of information:
    --
    --   - the type that the type family application reduces to,
    --   - evidence for this reduction, i.e. a 'GHC.Core.TyCo.Rep.Coercion' proving the equality.
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
  , isTyVarTy, getTyVar_maybe
  , TcType, TcTyVar, Unique, Kind

    -- ** Type literals (natural numbers, type-level strings)
  , mkNumLitTy, isNumLitTy
  , mkStrLitTy, isStrLitTy
  , natKind, symbolKind, charKind

    -- ** Creating and decomposing applications
  , mkTyConTy, mkTyConApp, mkAppTy, mkAppTys
  , splitTyConApp_maybe
  , tyConAppTyConPicky_maybe, tyConAppTyCon_maybe
  , splitAppTy_maybe, splitAppTys
  , isNewTyCon, dataConTyCon

    -- ** Data constructors
  , tyConDataCons, tyConSingleDataCon_maybe, tyConSingleDataCon
  , isNewDataCon

    -- ** Function types
  , mkVisFunTyMany, mkVisFunTysMany
  , mkInvisFunTy, mkInvisFunTys
  , mkForAllTy, mkForAllTys
  , mkPiTy, mkPiTys

    -- ** Kinds
  , typeKind

#if MIN_VERSION_ghc(9,0,0)
  , Mult
  , pattern OneTy, pattern ManyTy
#endif

    -- ** Zonking

    -- | Zonking is the operation in which GHC actually switches out mutable unification variables
    -- for their actual filled in type.
    --
    -- See the Note [What is zonking?] in GHC's source code for more information.
  , zonkTcType
  , zonkCt

    -- ** Panicking

    -- | It is often better for type-checking plugins to panic when encountering a problem,
    -- as opposed to silently doing something wrong. Use 'pprPanic' to throw an informative
    -- error message, so that users of your plugin can report an issue if a problem occurs.
  , panic, pprPanic

    -- ** Map-like data structures based on 'Unique's

    -- | Import "GHC.Types.Unique.FM" or "GHC.Types.Unique.DFM" for
    -- a more complete interface to maps whose keys are 'Unique's.

  , UniqDFM
  , lookupUDFM, lookupUDFM_Directly, elemUDFM
  , UniqFM
  , emptyUFM, listToUFM

    -- * The type-checking environment
  , getEnvs

    -- * Interacting with GHC's constraint solver
  , TcS
  , InertSet, getInertSet, setInertSet
  , getTcEvBindsMap, setTcEvBindsMap

    -- * Built-in types

    -- | This module also re-exports the built-in types that GHC already knows about.
    --
    -- This allows plugins to directly refer to e.g. the promoted data constructor
    -- 'True' without having to look up its name.
    --
    -- Refer to "GHC.Builtin.Names", "GHC.Builtin.Types" and "GHC.Builtin.Types.Prim".

    -- * GHC types

    -- | These are the types that the plugin will inspect and manipulate.

    -- | = END OF API DOCUMENTATION, RE-EXPORTS FOLLOW

    -- | == Some basic types

  , module GHC.Types.Basic

    -- | == Names
  , Name, OccName, TyThing, TcTyThing
  , MonadThings(..)
  , Class(classTyCon), DataCon, TyCon, Id
  , FastString

    -- | == Constraints
  , EqRel(..), FunDep, CtFlavour
  , Ct, CtLoc, CtEvidence, CtOrigin
  , QCInst
  , Type, PredType
  , InstEnvs, TcLevel

    -- | === Coercions and evidence
  , Coercion, Role(..), UnivCoProvenance
  , CoercionHole(..)
  , EvBind, EvTerm(EvExpr), EvVar, EvExpr, EvBindsVar
  , Expr(Var, Type, Coercion), CoreBndr, CoreExpr
  , TcEvDest(..)

    -- | == The type-checking environment
  , TcGblEnv, TcLclEnv

    -- | == Source locations
  , GenLocated(..), Located, RealLocated
  , unLoc, getLoc

    -- | == Pretty-printing
  , SDoc, Outputable(..)

  )
  where

-- base
import Prelude
  hiding ( cos )
#if !MIN_VERSION_ghc(8,11,0)
import Data.List.NonEmpty
  ( NonEmpty(..) )
#endif

-- ghc
import GHC
  ( TyThing(..) )
import GHC.Builtin.Names
 ( hasKey
 , eqPrimTyConKey, eqReprPrimTyConKey
 , heqTyConKey, eqTyConKey, coercibleTyConKey
 )
import GHC.Builtin.Types
  ( typeSymbolKind, charTy
#if MIN_VERSION_ghc(9,1,0)
  , naturalTy
#else
  , typeNatKind
#endif
  )
#if !MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types
  ( intDataCon )
import GHC.Builtin.Types.Prim
  ( intPrimTy )
#endif
import GHC.Core
  ( CoreBndr, CoreExpr, Expr(..), mkTyApps, mkApps )
import GHC.Core.Class
  ( Class(..), FunDep )
import GHC.Core.Coercion
  ( mkReflCo, mkSymCo, mkTransCo
  , mkUnivCo, isReflCo
#if !MIN_VERSION_ghc(9,13,0) && MIN_VERSION_ghc(8,10,0)
  , mkPrimEqPredRole
#endif
  )
import GHC.Core.Coercion.Axiom
  ( Role(..) )
import GHC.Core.DataCon
  ( DataCon
  , classDataCon, promoteDataCon, dataConWrapId
  , dataConTyCon
#if MIN_VERSION_ghc(9,1,0)
  , isNewDataCon
#endif
  )
import GHC.Core.FamInstEnv
  ( FamInstEnv )
import GHC.Core.InstEnv
  ( InstEnvs(..) )
#if !MIN_VERSION_ghc(9,0,0)
import GHC.Core.Make
  ( mkCoreConApps )
#endif
import GHC.Core.Predicate
  ( EqRel(..)
#if MIN_VERSION_ghc(9,13,0)
  , mkEqPredRole
#endif
#if MIN_VERSION_ghc(8,10,0)
  , Pred(..)
#else
  , PredTree(..), TyCoBinder
  , mkPrimEqPred, mkReprPrimEqPred
#endif
  , classifyPredType, mkClassPred
  )
#if HAS_REWRITING
import GHC.Core.Reduction
  ( Reduction(..) )
#endif
import GHC.Core.TyCon
  ( TyCon(..), tyConClass_maybe
  , tyConDataCons, tyConSingleDataCon_maybe, tyConSingleDataCon
  , isNewTyCon
  )
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCo.Compare
  ( eqType )
#endif
import GHC.Core.TyCo.Rep
  ( Type, PredType, Kind
  , Coercion(..), CoercionHole(..)
  , UnivCoProvenance(..)
#if MIN_VERSION_ghc(9,0,0)
  , Mult
  , mkVisFunTyMany, mkVisFunTysMany
#if MIN_VERSION_ghc(9,6,0)
  , mkInvisFunTy, mkInvisFunTys
#else
  , mkInvisFunTyMany, mkInvisFunTysMany
#endif
#elif MIN_VERSION_ghc(8,10,0)
  , mkVisFunTy, mkVisFunTys
  , mkInvisFunTy, mkInvisFunTys
#else
  , mkFunTy, mkFunTys
#endif
#if MIN_VERSION_ghc(8,10,0)
  , mkPiTy
#endif
  , mkPiTys
  , mkTyVarTy, mkTyVarTys
  , mkForAllTy, mkForAllTys
  )
import GHC.Core.Type
  ( mkTyConTy, mkTyConApp, splitTyConApp_maybe
  , splitAppTy_maybe, splitAppTys
  , tyConAppTyConPicky_maybe, tyConAppTyCon_maybe
  , mkAppTy, mkAppTys, isTyVarTy, getTyVar_maybe
  , mkCoercionTy, isCoercionTy, isCoercionTy_maybe
  , mkNumLitTy, isNumLitTy, mkStrLitTy, isStrLitTy
  , typeKind
#if !MIN_VERSION_ghc(9,6,0)
  , eqType
#endif
#if MIN_VERSION_ghc(9,6,0)
  , pattern OneTy, pattern ManyTy
#elif MIN_VERSION_ghc(9,0,0)
  , pattern One, pattern Many
#endif
  )
import GHC.Data.FastString
  ( FastString, fsLit, unpackFS )
#if MIN_VERSION_ghc(9,5,0)
import GHC.Plugins
  ( thNameToGhcNameIO )
#else
import Data.Maybe
  ( listToMaybe )
#if MIN_VERSION_ghc(9,3,0)
import GHC.Iface.Env
  ( lookupNameCache )
#else
import GHC.Iface.Env
  ( lookupOrigIO )
#endif
import GHC.ThToHs
  ( thRdrNameGuesses )
import GHC.Types.Name
  ( isExternalName )
import GHC.Types.Name.Reader
  ( isExact_maybe, isOrig_maybe )
import GHC.Utils.Monad
  ( mapMaybeM )
#endif
import qualified GHC.Tc.Plugin
  as GHC
#if MIN_VERSION_ghc(9,4,0)
import GHC.Tc.Solver.InertSet
  ( InertSet )
#endif
import GHC.Tc.Solver.Monad
  ( TcS
#if !MIN_VERSION_ghc(9,4,0)
  , InertSet
#endif
#if MIN_VERSION_ghc(9,8,0)
  , getInertSet, updInertSet
#else
  , getTcSInerts, setTcSInerts
#endif
  , getTcEvBindsMap, setTcEvBindsMap
  )
import GHC.Tc.Types
  ( TcTyThing(..), TcGblEnv(..), TcLclEnv(..)
#if HAS_REWRITING
  , TcPluginSolveResult(..), TcPluginRewriteResult(..)
  , RewriteEnv(..)
#endif
  )
#if MIN_VERSION_ghc(9,11,0)
import GHC.Tc.Types.CtLoc
  ( CtLoc(..), bumpCtLocDepth )
#else
import GHC.Tc.Types.Constraint
  ( CtLoc(..), bumpCtLocDepth )
#endif
#if MIN_VERSION_ghc(9,13,0)
import GHC.Tc.Types.Constraint
  ( GivenCtEvidence(..) )
#endif
import GHC.Tc.Types.Constraint
  ( Ct(..), CtEvidence(..), CtFlavour(..)
  , QCInst(..), TcEvDest(..)
  , ctPred, ctLoc, ctEvidence, ctEvExpr
  , ctEvCoercion
  , ctFlavour, ctEqRel, ctOrigin
  , ctEvFlavour, ctEvPred, ctEvLoc
  , mkNonCanonical, ctEvId
  )
import GHC.Tc.Types.Evidence
  ( EvBind(..), EvTerm(..), EvExpr, EvBindsVar(..)
  , evCoercion, lookupEvBind
  , mkGivenEvBind, evId
  )
import GHC.Tc.Types.Origin
  ( CtOrigin(..) )
import GHC.Tc.Utils.Monad
  ( newName, readTcRef, writeTcRef )
import qualified GHC.Tc.Utils.Monad
  as GHC
    ( traceTc, setCtLocM )
import GHC.Tc.Utils.TcType
  ( TcType, TcLevel, MetaDetails, MetaInfo
  , isSkolemTyVar, isMetaTyVar
  , nonDetCmpType
  )
import GHC.Tc.Utils.TcMType
  ( isFilledMetaTyVar_maybe, writeMetaTyVar )
import GHC.Types.Basic
  ( Arity, PromotionFlag(..), isPromoted
  , Boxity(..), TupleSort(..)
  )
import GHC.Types.Id
  ( Id, mkLocalId )
#if !MIN_VERSION_ghc(9,0,0)
import GHC.Types.Literal
  ( Literal(..), LitNumType(..) )
#endif
import GHC.Types.Name
  ( Name )
import GHC.Types.Name.Occurrence
  ( OccName(..)
  , mkVarOcc, mkDataOcc, mkTyVarOcc, mkTcOcc, mkClsOcc
  )
#if MIN_VERSION_ghc(9,1,0)
import GHC.Driver.Env.Types
  ( HscEnv )
#else
import GHC.Driver.Types
  ( HscEnv )
#endif
#if MIN_VERSION_ghc(9,3,0)
import GHC.Driver.Env.Types
  ( hsc_unit_env, hsc_NC )
import GHC.Types.PkgQual
  ( PkgQual(..) )
import GHC.Rename.Names
  ( renamePkgQual )
import GHC.Unit.Module
  ( unitIdString )
#elif MIN_VERSION_ghc(9,1,0)
import GHC.Data.FastString
  ( NonDetFastString(NonDetFastString) )
#endif
import GHC.Types.SrcLoc
  ( GenLocated(..), Located, RealLocated
  , unLoc, getLoc
  )
import GHC.Types.Unique
  ( Unique )
#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Unique.FM as UniqFM
  ( UniqFM, emptyUFM, listToUFM )
#else
import qualified GHC.Types.Unique.FM as GHC
  ( UniqFM )
import GHC.Types.Unique.FM as UniqFM
  ( emptyUFM, listToUFM )
#endif
import GHC.Types.Unique.DFM
  ( UniqDFM, lookupUDFM, lookupUDFM_Directly, elemUDFM )
import GHC.Types.Var
  ( TyVar, CoVar, TcTyVar, EvVar
  , mkTyVar, DFunId
  )
import GHC.Utils.Outputable
  ( Outputable(..), SDoc, text )
#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Panic
  ( panic, pprPanic )
#else
import GHC.Utils.Outputable
  ( panic, pprPanic )
#endif
#if MIN_VERSION_ghc(9,2,0)
import GHC.Unit.Finder
  ( FindResult(..) )
#else
import GHC.Driver.Finder
  ( FindResult(..) )
#endif
import GHC.Unit.Module
  ( UnitId, unitIdFS, stringToUnitId, mkModuleName )
#if MIN_VERSION_ghc(9,5,0)
import Language.Haskell.Syntax.Module.Name
  ( ModuleName )
#else
import GHC.Unit.Module.Name
  ( ModuleName )
#endif
import GHC.Unit.Types
  ( Module )
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,5,0)
import GHC.Utils.Misc
  ( HasDebugCallStack )
#endif

-- ghc-tcplugin-api
import GHC.TcPlugin.API.Internal
#ifndef HAS_REWRITING
import GHC.TcPlugin.API.Internal.Shim
#endif

-- template-haskell
import qualified Language.Haskell.TH as TH

-- transformers
import Control.Monad.IO.Class
  ( MonadIO ( liftIO ) )

--------------------------------------------------------------------------------

-- | Output some debugging information within the plugin.
tcPluginTrace :: MonadTcPlugin m
              => String -- ^ Text at the top of the debug message.
              -> SDoc   -- ^ Formatted document to print (use the 'ppr' pretty-printing function to obtain an 'SDoc' from any 'Outputable')
              -> m ()
tcPluginTrace a b = unsafeLiftTcM $ GHC.traceTc a b

--------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,5,0)
pattern OneTy, ManyTy :: Mult
pattern OneTy = One
pattern ManyTy = Many

mkInvisFunTy :: HasDebugCallStack => Type -> Type -> Type
mkInvisFunTy = mkInvisFunTyMany

mkInvisFunTys :: HasDebugCallStack => [Type] -> Type -> Type
mkInvisFunTys = mkInvisFunTysMany
#endif

--------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,3,0)

-- Use PkgQual from ghc itself

#elif MIN_VERSION_ghc(9,1,0)

newtype PkgQual = PkgQual (Maybe NonDetFastString)
  deriving stock ( Eq, Ord )
  deriving newtype ( Outputable )

-- | INTERNAL: Unwrap 'PkgQual'
getPkgQual :: PkgQual -> Maybe FastString
getPkgQual (PkgQual Nothing)                       = Nothing
getPkgQual (PkgQual (Just (NonDetFastString pkg))) = Just pkg

#else

newtype PkgQual = PkgQual (Maybe FastString)
  deriving stock ( Eq, Ord )
  deriving newtype ( Outputable )

-- | INTERNAL: Unwrap 'PkgQual'
getPkgQual :: PkgQual -> Maybe FastString
getPkgQual (PkgQual mPkg) = mPkg

#endif

-- | Get package name from package qualifier
pkgQualToPkgName :: PkgQual -> Maybe String
#if MIN_VERSION_ghc(9,3,0)
pkgQualToPkgName NoPkgQual       = Nothing
pkgQualToPkgName (ThisPkg  unit) = Just $ unitIdString unit
pkgQualToPkgName (OtherPkg unit) = Just $ unitIdString unit
#else
pkgQualToPkgName = fmap unpackFS . getPkgQual
#endif

-- | Resolve import
resolveImport :: MonadTcPlugin m
              => ModuleName        -- ^ Module name to import from
              -> Maybe FastString  -- ^ Optional package qualifier
              -> m PkgQual
#if MIN_VERSION_ghc(9,3,0)
resolveImport mod_name mPkg = do
  hscEnv <- getTopEnv
  return $ renamePkgQual (hsc_unit_env hscEnv) mod_name mPkg
#elif MIN_VERSION_ghc(9,1,0)
resolveImport _mod_name mPkg = do
  return $ PkgQual (NonDetFastString <$> mPkg)
#else
resolveImport _mod_name mPkg = do
  return $ PkgQual mPkg
#endif

-- | Lookup a Haskell module, with an optional package qualifier.
findImportedModule :: MonadTcPlugin m
                   => ModuleName  -- ^ Module name, e.g. @"Data.List"@
                   -> PkgQual     -- ^ Package qualifier. See 'resolveImport'
                   -> m FindResult
#if MIN_VERSION_ghc(9,3,0)
findImportedModule mod_name pkg = liftTcPluginM $
    GHC.findImportedModule mod_name pkg
#else
findImportedModule mod_name pkg = liftTcPluginM $
    GHC.findImportedModule mod_name (getPkgQual pkg)
#endif

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

getTopEnv :: MonadTcPlugin m => m HscEnv
getTopEnv = liftTcPluginM GHC.getTopEnv

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
  fmap ( \ (co,ty) -> mkReduction (mkSymCo co) ty ) <$>
  -- GHC 9.0 and 9.2 use a different orientation
  -- when rewriting type family applications.
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
zonkTcType :: MonadTcPluginWork m => TcType -> m TcType
zonkTcType = liftTcPluginM . GHC.zonkTcType

-- | Zonk a given constraint.
zonkCt :: MonadTcPluginWork m => Ct -> m Ct
zonkCt = liftTcPluginM . GHC.zonkCt

--------------------------------------------------------------------------------

-- | Is this a "Given" constraint?
isGiven :: CtEvidence -> Bool
isGiven ev = case ctEvFlavour ev of
  Given {} -> True
  _ -> False

-- | Is this a "Wanted" constraint?
isWanted :: CtEvidence -> Bool
isWanted ev = case ctEvFlavour ev of
  Wanted {} -> True
  _ -> False

--------------------------------------------------------------------------------

-- | Create a new Wanted constraint.
--
-- Requires a location (so that error messages can say where the constraint came from,
-- what things were in scope at that point, etc), as well as the actual constraint (encoded as a type).
newWanted :: MonadTcPluginWork m => CtLoc -> PredType -> m CtEvidence
newWanted loc pty =
#if !HAS_REWRITING
  -- On GHC 9.2 and below, 'newWanted' doesn't use the location information
  -- that is passed to it, retrieving it from the 'TcM' environment instead.
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/20895
  setCtLocM loc $
#endif
  liftTcPluginM $ GHC.newWanted loc pty

-- | Create a new Given constraint.
--
-- Unlike 'newWanted', we need to supply evidence for this constraint.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM Solve CtEvidence
newGiven loc pty evtm = do
   new_ev <- newEvVar pty
   setEvBind $ mkGivenEvBind new_ev evtm
   return $
      CtGiven
#if MIN_VERSION_ghc(9,13,0)
        $ GivenCt
#endif
        { ctev_pred = pty
        , ctev_evar = new_ev
        , ctev_loc = loc
        }

-- | Obtain the 'CtLoc' from a 'RewriteEnv'.
--
-- This can be useful to obtain the location of the
-- constraint currently being rewritten,
-- so that newly emitted constraints can be given
-- the same location information.
rewriteEnvCtLoc :: RewriteEnv -> CtLoc
rewriteEnvCtLoc =
#if MIN_VERSION_ghc(9,3,0)
  re_loc
#else
  fe_loc
#endif

-- | Set the location information for a computation.
setCtLocM :: MonadTcPluginWork m => CtLoc -> m a -> m a
setCtLocM loc = unsafeLiftThroughTcM ( GHC.setCtLocM loc )

-- | Use the 'RewriteEnv' to set the 'CtLoc' for a computation.
setCtLocRewriteM :: TcPluginM Rewrite a -> TcPluginM Rewrite a
setCtLocRewriteM ma = do
  rewriteCtLoc <- rewriteEnvCtLoc <$> askRewriteEnv
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
--
-- This amounts to telling GHC "believe me, these things are equal".
--
-- The plugin is responsible for not emitting any unsound coercions,
-- such as a coercion between 'Int' and 'Float'.
mkPluginUnivCo
  :: String  -- ^ Name of equality (for the plugin's internal use, or for debugging)
  -> Role
  -> [Coercion] -- ^ Evidence that this proof term depends on (use 'ctEvCoercion')
  -> TcType  -- ^ LHS
  -> TcType  -- ^ RHS
  -> Coercion
mkPluginUnivCo str role _deps lhs rhs =
  mkUnivCo
    ( PluginProv str )
#if MIN_VERSION_ghc(9,12,0)
    _deps
#endif
    role
    lhs
    rhs

-- | Conjure up an evidence term for an equality between two types
-- at the given 'Role' ('Nominal' or 'Representational').
--
-- This can be used to supply a proof of a wanted equality in 'TcPluginOk'.
--
-- The plugin is responsible for not emitting any unsound equalities,
-- such as an equality between 'Int' and 'Float'.
mkPluginUnivEvTerm
  :: String     -- ^ Name of equality (for the plugin's internal use, or for debugging)
  -> Role
  -> [Coercion] -- ^ Evidence that this proof term depends on (use 'ctEvCoercion')
  -> TcType     -- ^ LHS
  -> TcType     -- ^ RHS
  -> EvTerm
mkPluginUnivEvTerm str role deps lhs rhs =
  evCoercion $ mkPluginUnivCo str role deps lhs rhs

-- | Provide a rewriting of a saturated type family application
-- at the given 'Role' ('Nominal' or 'Representational').
--
-- The result can be passed to 'TcPluginRewriteTo' to specify the outcome
-- of rewriting a type family application.
mkTyFamAppReduction
  :: String     -- ^ Name of reduction (for debugging)
  -> Role       -- ^ Role of reduction ('Nominal' or 'Representational')
  -> [Coercion] -- ^ Evidence that this reduction depends on (use 'ctEvCoercion')
  -> TyCon      -- ^ Type family 'TyCon'
  -> [TcType]   -- ^ Type family arguments
  -> TcType     -- ^ The type that the type family application reduces to
  -> Reduction
mkTyFamAppReduction str role deps tc args ty =
  Reduction
    ( mkPluginUnivCo str role deps ( mkTyConApp tc args ) ty )
    ty

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,0,0)

type UniqFM ty a = GHC.UniqFM a

mkUncheckedIntExpr :: Integer -> CoreExpr
mkUncheckedIntExpr i = mkCoreConApps intDataCon [Lit lit]
  where
    lit = LitNumber LitNumInt i intPrimTy

#if MIN_VERSION_ghc(8,10,0)

mkVisFunTyMany :: Type -> Type -> Type
mkVisFunTyMany  = mkVisFunTy

mkVisFunTysMany :: [Type] -> Type -> Type
mkVisFunTysMany = mkVisFunTys

#else

type Pred = PredTree

mkInvisFunTy, mkVisFunTyMany  :: Type -> Type -> Type
mkInvisFunTy   = mkFunTy
mkVisFunTyMany = mkFunTy

mkInvisFunTys, mkVisFunTysMany :: [Type] -> Type -> Type
mkInvisFunTys   = mkFunTys
mkVisFunTysMany = mkFunTys

mkPiTy :: TyCoBinder -> Type -> Type
mkPiTy bndr ty = mkPiTys [bndr] ty

#endif
#endif

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,13,0)
-- | Makes an unlifted equality predicate at the given role
-- (either 'Nominal' or 'Representational').
mkEqPredRole :: Role -> Type -> Type -> PredType
#if MIN_VERSION_ghc(8,10,0)
mkEqPredRole = mkPrimEqPredRole
#else
mkEqPredRole Nominal          = mkPrimEqPred
mkEqPredRole Representational = mkReprPrimEqPred
mkEqPredRole Phantom          = panic "mkPrimEqPredRole phantom"
#endif
#endif

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,1,0)
isNewDataCon :: DataCon -> Bool
isNewDataCon dc = isNewTyCon (dataConTyCon dc)
#endif

#if !MIN_VERSION_ghc(8,11,0)
instance Outputable a => Outputable (NonEmpty a) where
  ppr (x :| xs) = ppr (x : xs)
#endif

--------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,8,0)
setInertSet :: InertSet -> TcS ()
setInertSet inerts = updInertSet ( const inerts )
  -- workaround for setInertSet not being exported

#elif !MIN_VERSION_ghc(9,8,0)
getInertSet :: TcS InertSet
getInertSet = getTcSInerts

setInertSet :: InertSet -> TcS ()
setInertSet = setTcSInerts
#endif

--------------------------------------------------------------------------------

-- | Resolve a @template-haskell@ 'TH.Name' to a GHC 'Name'.
--
-- Especially useful in conjunction with @TemplateHaskellQuotes@, e.g. to
-- look up the 'Name' of the @Maybe@ type constructor, you can do:
--
--  > lookupName ''Maybe
--
-- @since 0.15.0.0
lookupTHName :: ( Monad (TcPluginM s), MonadTcPlugin (TcPluginM s) )
             => TH.Name -> TcPluginM s Name
lookupTHName thNm = do
  hscEnv <- getTopEnv
  mbNm <-
    liftIO $
      thNameToGhcNameIO
#if MIN_VERSION_ghc(9,6,0)
        (hsc_NC hscEnv)
#else
        hscEnv
#endif
        thNm
  case mbNm of
    Just nm ->
      return nm
    Nothing ->
      pprPanic "lookupTHName: lookup failed" (text $ show thNm)

--------------------------------------------------------------------------------

-- | Apply an instance dictionary function to type and value arguments.
--
-- NB: different from @evDFunApp@ defined in GHC, as this function returns
-- an @EvExpr@ rather than an @EvTerm@.
evDFunApp :: DFunId -> [Type] -> [EvExpr] -> EvExpr
evDFunApp df tys ets = Var df `mkTyApps` tys `mkApps` ets

-- | Apply a data constructor to type and value arguments.
--
-- NB: different from @evDataConApp@ defined in GHC, as this function returns
-- an @EvExpr@ rather than an @EvTerm@.
evDataConApp :: DataCon -> [Type] -> [EvExpr] -> EvExpr
evDataConApp dc tys ets = evDFunApp (dataConWrapId dc) tys ets

-- | Cast an evidence expression by a coercion.
--
-- NB: different from @evCast@ defined in GHC, as this function returns
-- an @EvExpr@ rather than an @EvTerm@.
evCast :: EvExpr -> Coercion -> EvExpr
evCast et co | isReflCo co = et
             | otherwise   = Cast et co

--------------------------------------------------------------------------------

natKind, symbolKind, charKind :: Type
natKind =
#if MIN_VERSION_ghc(9,1,0)
  naturalTy
#else
  typeNatKind
#endif
symbolKind = typeSymbolKind
charKind = charTy

--------------------------------------------------------------------------------

-- | Is this a primitive (unboxed) equality type, i.e. one of:
--
--  - @~#@ (heterogeneous nominal unboxed equality),
--  - @~R#@ (heterogeneous representational unboxed equality).
--
-- NB: returns @False@ for **boxed** equalities such as @(~)@, @(~~)@ and @Coercible@.
-- If you want to detect those, use 'isEqClassPred'.
isEqPred :: PredType -> Bool
isEqPred pty
  | Just tc <- tyConAppTyCon_maybe pty
  = tc `hasKey` eqPrimTyConKey || tc `hasKey` eqReprPrimTyConKey
  | otherwise
  = False

-- | Is this a boxed equality type, i.e. one of:
--
--  - @(~)@ (homogeneous nominal boxed equality),
--  - @(~~)@ (heterogeneous nominal boxed equality),
--  - @Coercible@ (homogeneous boxed representational equality).
isEqClassPred :: PredType -> Bool
isEqClassPred pty
  | Just tc <- tyConAppTyCon_maybe pty
  , Just cls <- tyConClass_maybe tc
  =    cls `hasKey` heqTyConKey
    || cls `hasKey` eqTyConKey
    || cls `hasKey` coercibleTyConKey
  | otherwise
  = False

--------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,6,0)
thNameToGhcNameIO :: HscEnv -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO hscEnv th_name
  =  do { names <- mapMaybeM do_lookup (thRdrNameGuesses th_name)
        ; return (listToMaybe names) }
  where
    do_lookup rdr_name
      | Just n <- isExact_maybe rdr_name
      = return $ if isExternalName n then Just n else Nothing
      | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
#if MIN_VERSION_ghc(9,3,0)
      = Just <$> lookupNameCache (hsc_NC hscEnv) rdr_mod rdr_occ
#else
      = Just <$> lookupOrigIO hscEnv rdr_mod rdr_occ
#endif
      | otherwise
      = return Nothing
#endif

--------------------------------------------------------------------------------
