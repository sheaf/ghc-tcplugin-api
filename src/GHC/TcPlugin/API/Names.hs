{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if MIN_VERSION_ghc(9,0,0)
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

{-|
Module: GHC.TcPlugin.API.Names

This module provides an /optional/ framework that facilitates name lookup
in type-checking plugins, using constrained traversals (similar to the
<https://hackage.haskell.org/package/barbies barbies library>).

See the 'ResolveNames' typeclass.

Before:

> data PluginDefs =
>   PluginDefs
>     { myTyCon           :: TyCon
>     , myClass           :: Class
>     , myPromotedDataCon :: TyCon
>     }
>
> findMyModule :: MonadTcPlugin m => m Module
> findMyModule = do
>   findResult <- findImportedModule ( mkModuleName "MyModule" ) Nothing
>   case findResult of
>     Found _ res -> pure res
>     _           -> error $ "MyPlugin: could not find any module named MyModule."
>
> pluginInit :: TcPluginM Init PluginDefs
> pluginInit = do
>   myModule <- findMyModule
>   myTyCon           <-                       tcLookupTyCon   =<< lookupOrig myModule ( mkTcOcc   "MyTyCon"   )
>   myClass           <-                       tcLookupClass   =<< lookupOrig myModule ( mkClsOcc  "MyClass"   )
>   myPromotedDataCon <- fmap promoteDataCon . tcLookupDataCon =<< lookupOrig myModule ( mkDataOcc "MyDataCon" )
>   pure ( PluginDefs { .. } )

After:

> data PluginDefsHKD n =
>   PluginDefs
>     { myTyCon            :: Wear n TyCon
>     , myClass            :: Wear n Class
>     , myPromotedDataCon  :: Wear n ( Promoted DataCon )
>     }
>   deriving stock Generic
>   deriving ResolveNames
>     via Generically1 PluginDefsHKD
>
> type PluginDefs = PluginDefsHKD Resolved
>
> pluginInit :: TcPluginM Init PluginDefs
> pluginInit = resolveNames pluginNames
>   where
>     pluginNames :: PluginDefsHKD Named
>     pluginNames =
>       PluginDefs
>         { myTyCon           = mkQualified "MyTyCon"
>         , myClass           = mkQualified "MyClass"
>         , myPromotedDataCon = mkQualified "MyDataCon"
>         }
>     mkQualified :: String -> QualifiedName thing
>     mkQualified str =
>       Qualified
>         { name    = str
>         , module' = mkModuleName "MyModule"
>         , package = Nothing
>         }

-}

module GHC.TcPlugin.API.Names
  ( ResolveNames, resolveNames
  , Wear, QualifiedName(..), NameResolution(..)
  , Promoted
  , Lookupable(..)

    -- * Re-export Generically1 for compatibility.
  , Generically1(..)
  ) where

-- base
import Prelude
  hiding ( lookup )
import Data.Coerce
  ( Coercible, coerce )
import Data.Kind
  ( Type, Constraint )
import GHC.Generics
  ( Generic(..)
#if MIN_VERSION_base(4,17,0)
  , Generically1(..)
#endif
  , (:+:)(..), (:*:)(..)
  , K1(K1), M1(M1), U1(..), V1, Rec0
  )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- transformers
import Control.Monad.Trans.State.Strict
  ( StateT, evalStateT, get, put )
import Control.Monad.Trans.Class
  ( MonadTrans(lift) )

-- ghc
#if MIN_VERSION_ghc(9,3,0)
import GHC.Iface.Errors
  ( cannotFindModule )
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Iface.Load
  ( cannotFindModule )
#else
import GHC.Driver.Types
  ( hsc_dflags )
import GHC.Driver.Finder
  ( cannotFindModule )
import GHC.Driver.Session
  ( DynFlags )
#endif
import GHC.Utils.Panic
  ( pgmErrorDoc )
import GHC.Unit.Module.Name
  ( moduleNameString )
import GHC.Tc.Plugin
  ( getTopEnv )
import GHC.Types.Unique.FM
  ( addToUFM, addToUFM_C, lookupUFM, plusUFM, unitUFM )

-- ghc-tcplugin-api
import GHC.TcPlugin.API
  hiding ( Type )
import GHC.TcPlugin.API.Internal
  ( MonadTcPlugin(liftTcPluginM) )

--------------------------------------------------------------------------------

-- | A 'QualifiedName' is the name of something,
-- together with the names of the module and package it comes from.
data QualifiedName (thing :: Type)
  = Qualified
    { -- | Name of the thing (e.g. name of the 'TyCon' or 'Class').
      name    :: String
      -- | Name of the module in which the thing can be found.
    , module' :: ModuleName
      -- | Name of the package in which the module can be found.
      -- Use 'Nothing' to signify the current home package.
    , package :: Maybe FastString
    }

-- | Type-level parameter to 'Wear' type family, for higher-kinded data.
--
-- @Wear Named thing@ is the identifier data passed in as an argument.
-- @Wear Resolved thing@ is the result of name resolving the thing.
--
-- This allows users to pass a record of names, of type @MyData Named@,
-- and obtain a record of looked-up things, of type @MyData Resolved@.
--
-- Refer to 'ResolveNames' for a worked example.
data NameResolution = Named | Resolved

-- | Use this to refer to a @Promoted DataCon@.
data Promoted (thing :: k) :: Type

-- | Type-family used for higher-kinded data pattern.
--
-- This allows the same record to be re-used,
-- as explained in the worked example for 'ResolveNames'.
--
-- For instance, if one defines:
--
-- > data MyData n
-- >   = MyData
-- >   { myClass :: !( Wear n Class )
-- >   , myTyCon :: !( Wear n TyCon )
-- >   }
--
-- then a record of type @MyData Named@ is simply a record of textual names
-- (a typeclass name and a type-constructor name, with associated module & packages),
-- whereas a record of type @MyData Resolved@ contains a typeclass's @Class@
-- as well as a type-constructor's @TyCon@.
#if MIN_VERSION_ghc(9,0,0)
type Wear :: forall k. NameResolution -> k -> Type
#endif
type family Wear (n :: NameResolution) (thing :: k) :: Type where
#if MIN_VERSION_ghc(9,0,0)
  Wear @Type Named thing           = QualifiedName thing
#else
  Wear Named thing                 = QualifiedName thing
#endif
  Wear Resolved (Promoted DataCon) = TyCon
  Wear Resolved (Promoted a)
    = TypeError
      ( Text "Cannot promote " :<>: ShowType a :<>: Text "."
      :$$: Text "Can only promote 'DataCon's."
      )
  Wear Resolved thing = thing

-- | Retrieve the underlying thing being referred to by inspecting
-- the type parameter of 'QualifiedName'.
type family UnwearNamed (loc :: Type) :: Type where
  UnwearNamed (QualifiedName thing) = thing

-- | Type-class overloading things that can be looked up by name:
--
-- * classes,
-- * data constructors (as well as their promotion),
-- * type-constructors.
#if MIN_VERSION_ghc(9,0,0)
type Lookupable :: forall {k}. k -> Constraint
#endif
class Lookupable (a :: k) where
  mkOccName :: String -> OccName
  lookup :: MonadTcPlugin m => Name -> m (Wear Resolved a)

instance Lookupable TyCon where
  mkOccName = mkTcOcc
  lookup = tcLookupTyCon
instance Lookupable DataCon where
  mkOccName = mkDataOcc
  lookup = tcLookupDataCon
instance Lookupable Class where
  mkOccName = mkClsOcc
  lookup = tcLookupClass
instance Lookupable (Promoted DataCon) where
  mkOccName = mkDataOcc
  lookup = fmap promoteDataCon . tcLookupDataCon

-- | This class exposes the method 'resolveNames' which will
-- perform name resolution for all the fields in a datatype.
--
-- Example usage: we define a record that will hold
-- the things we want to look up, using the 'Wear' type family.
--
-- For example:
--
--  > data MyData n
--  >   = MyData
--  >   { myClass       :: !( Wear n Class )
--  >   , myTyCon       :: !( Wear n TyCon )
--  >   , myDataCon     :: !( Wear n DataCon )
--  >   , myPromDataCon :: !( Wear n (Promoted DataCon) )
--  >   }
--  >   deriving stock Generic
--  >   deriving ResolveNames
--  >     via Generically1 MyData
--
-- Now we can specify the names of the things which we want to look up,
-- together with the modules and packages in which they belong:
--
-- > myNames :: MyData Named
-- > myNames = MyData
-- >  { myClass = QualifiedName "MyClass" "My.Module" ( Just "my-pkg-name" )
-- >  , ...
-- >  }
--
-- Then we can call 'resolveNames':
--
-- > resolvedNames :: MonadTcPlugin m => m (MyData Resolved)
-- > resolvedNames = resolveNames myNames
--
-- This returns a record containing the looked up things we want,
-- e.g. @myClass :: Class@, @myPromDataCon :: TyCon@, etc.
class ResolveNames (f :: NameResolution -> Type) where
  resolve_names :: ( Coercible res ( f Resolved ), MonadTcPlugin m )
                => f Named -> m res
  -- Workaround: the result is anything coercible to "f Resolved" rather than just "f Resolved",
  -- because otherwise GHC complains when using DerivingVia that we don't know the role
  -- of the parameter to m, despite the quantified constraint superclass to MonadTcPlugin.
  --
  -- This unfortunately worsens type-inference, so we export
  -- 'resolveNames' separately.

-- | Resolve a collection of names.
--
-- See 'ResolveNames' for further details.
resolveNames :: ( MonadTcPlugin m, ResolveNames f )
             => f Named -> m ( f Resolved )
resolveNames = resolve_names

instance ( Generic (f Named)
         , Generic (f Resolved)
         , GTraversableC ResolveName (Rep (f Named)) (Rep (f Resolved))
         )
      => ResolveNames (Generically1 f) where
  resolve_names
    :: forall
#if MIN_VERSION_ghc(9,0,0)
         {m}
#else
          m
#endif
         res
    .  ( Coercible res ( Generically1 f Resolved ), MonadTcPlugin m )
    => Generically1 f Named -> m res
  resolve_names dat
    =  ( `evalStateT` emptyModules )
    $  coerce . to @(f Resolved)
   <$> gtraverseC @ResolveName resolveName ( from dat )

-- | Type-class dispatch for looking up names.
--
-- Every instance is of the form:
--
-- > ResolveName (Wear Named thing) (Wear Resolved thing)
--
-- which allows one to write 'resolveName':
--
-- > resolveName :: ... => Wear Named thing -> m ( Wear Resolved thing )
class    ( a ~ Wear Named    ( UnwearNamed a )
         , b ~ Wear Resolved ( UnwearNamed a )
         , Lookupable ( UnwearNamed a )
         )
      => ResolveName (a :: Type) (b :: Type)
instance ( a ~ Wear Named    ( UnwearNamed a )
         , b ~ Wear Resolved ( UnwearNamed a )
         , Lookupable ( UnwearNamed a )
         )
      => ResolveName a b

resolveName :: forall (thing :: Type) m
            .  ResolveName ( Wear Named thing ) ( Wear Resolved thing )
            => MonadTcPlugin m
            => Wear Named thing
            -> StateT ImportedModules m ( Wear Resolved thing )
resolveName (Qualified str mod_name mb_pkg) = do
  md <- lookupModule mb_pkg mod_name
  nm <- lift $ lookupOrig md
                 (mkOccName
#if !MIN_VERSION_ghc(9,0,0)
                   @_
#endif
                   @thing
                   str
                 )
  lift $ lookup
#if !MIN_VERSION_ghc(9,0,0)
           @_
#endif
           @thing nm

--------------------------------------------------------------------------------
-- Caching of found modules.

data ImportedModules
  = ImportedModules
    { home_modules :: UniqFM ModuleName Module
    , pkg_modules  :: UniqFM FastString ( UniqFM ModuleName Module )
    }

emptyModules :: ImportedModules
emptyModules = ImportedModules emptyUFM emptyUFM

lookupCachedModule :: Monad m => Maybe FastString -> ModuleName -> StateT ImportedModules m (Maybe Module)
lookupCachedModule Nothing    mod_name
  =   ( `lookupUFM` mod_name )
  .   home_modules
  <$> get
lookupCachedModule (Just pkg) mod_name
  =   ( ( `lookupUFM` mod_name ) =<< )
  .   ( `lookupUFM` pkg )
  .   pkg_modules
  <$> get

insertCachedModule :: Monad m => Maybe FastString -> ModuleName -> Module -> StateT ImportedModules m ()
insertCachedModule Nothing    mod_name md = do
  mods@( ImportedModules { home_modules = prev } ) <- get
  put $ mods { home_modules = addToUFM prev mod_name md }
insertCachedModule (Just pkg) mod_name md = do
  mods@( ImportedModules { pkg_modules = prev } ) <- get
  put $ mods { pkg_modules = addToUFM_C plusUFM prev pkg (unitUFM mod_name md) }

lookupModule :: MonadTcPlugin m => Maybe FastString -> ModuleName -> StateT ImportedModules m Module
lookupModule mb_pkg mod_name = do
  cachedResult <- lookupCachedModule mb_pkg mod_name
  case cachedResult of
    Just res -> do
      insertCachedModule mb_pkg mod_name res
      pure res
    Nothing -> do
      findResult <- lift $ findImportedModule mod_name mb_pkg
      case findResult of
        Found _ res
          -> pure res
        other -> do
          hsc_env <- lift . liftTcPluginM $ getTopEnv
          let
            err_doc :: SDoc
#if MIN_VERSION_ghc(9,2,0)
            err_doc = cannotFindModule hsc_env mod_name other
#else
            err_doc = cannotFindModule dflags  mod_name other
            dflags :: DynFlags
            dflags = hsc_dflags hsc_env
#endif
          pgmErrorDoc
            ( "GHC.TcPlugin.API: could not find module " <> mod_str <> " in " <> pkg_name )
            err_doc
  where
    pkg_name, mod_str :: String
    pkg_name = case mb_pkg of
      Just pkg -> "package " <> show pkg
      Nothing  -> "home package"
    mod_str = moduleNameString mod_name

--------------------------------------------------------------------------------
-- Constrained traversals.

type TraversalC (c :: Type -> Type -> Constraint) (s :: Type)  (t :: Type)
  =  forall f. ( Applicative f )
  => ( forall a b. c a b => a -> f b ) -> s -> f t

class GTraversableC (c :: Type -> Type -> Constraint) (s :: Type -> Type) (t :: Type -> Type) where
  gtraverseC :: TraversalC c (s x) (t x)

instance
  ( GTraversableC c l l'
  , GTraversableC c r r'
  ) => GTraversableC c (l :*: r) (l' :*: r') where
  gtraverseC f (l :*: r)
    = (:*:) <$> gtraverseC @c f l <*> gtraverseC @c f r

instance
  ( GTraversableC c l l'
  , GTraversableC c r r'
  ) => GTraversableC c (l :+: r) (l' :+: r') where
  gtraverseC f (L1 l) = L1 <$> gtraverseC @c f l
  gtraverseC f (R1 r) = R1 <$> gtraverseC @c f r

instance GTraversableC c s t
  => GTraversableC c (M1 i m s) (M1 i m t) where
  gtraverseC f (M1 x) = M1 <$> gtraverseC @c f x

instance GTraversableC c U1 U1 where
  gtraverseC _ _ = pure U1

instance GTraversableC c V1 V1 where
  gtraverseC _ = pure

instance c a b => GTraversableC c (Rec0 a) (Rec0 b) where
  gtraverseC f (K1 a) = K1 <$> f a

--------------------------------------------------------------------------------
-- Generically and Generically1 wrappers for DerivingVia.

#if !MIN_VERSION_base(4,17,0)
-- | A type whose instances are defined generically, using the
-- 'Generic1' representation. 'Generically1' is a higher-kinded
-- version of 'Generically' that uses 'Generic'.
--
-- Generic instances can be derived for type constructors via
-- @'Generically1' F@ using @-XDerivingVia@.
newtype Generically1 (f :: k -> Type) (a :: k) = Generically1 ( f a )
  deriving newtype Generic
#endif
