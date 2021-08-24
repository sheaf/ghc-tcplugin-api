{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -Wno-unused-foralls #-} -- for LitTy

module SystemF.Type where

-- base
import qualified Data.Kind as Hs
  ( Type )
import Prelude
  ( Show(..), ($) )
import qualified Prelude as Hs

--------------------------------------------------------------------------------
-- Kinds and types.

type Kind :: Hs.Type
data Kind where
  Type :: Kind
  Fun  :: Kind -> Kind -> Kind

infixl 4 :&*
-- | Type-level context, only keeping track of kinds.
type KContext :: Hs.Type
data KContext where
  KEmpty :: KContext
  (:&*)  :: KContext -> Kind -> KContext

-- | Type-level de Bruijn index.
type KIdx :: KContext -> Kind -> Hs.Type
data KIdx kϕ k where
  KZ :: KIdx (kϕ :&* k) k
  KS :: KIdx kϕ k -> KIdx (kϕ :&* l) k

infixr 0 :->
-- | A type, in the given context, of the given kind.
type Type :: KContext -> Kind -> Hs.Type
data Type kϕ k where
  LitTy  :: forall (a :: Hs.Type) kϕ. Type kϕ 'Type
  (:->)  :: Type kϕ k -> Type kϕ l -> Type kϕ (Fun k l)
  VarTy  :: KIdx kϕ k -> Type kϕ k
  Forall :: Type (kϕ :&* k) l -> Type kϕ l

type Int    = LitTy @Hs.Int
type String = LitTy @Hs.String

--------------------------------------
-- Renaming and substitution of types.

infixl 8 :.:
-- | A type renaming/substitution, which can be applied using 'ApplySub'.
type KSub :: KContext -> KContext -> Hs.Type
data KSub kϕ kψ where
  -- | Identity.
  KId     :: KSub kϕ kϕ
  -- | Bind a new variable at the top-level, increasing all other de Bruijn indices.
  KBind   :: forall kϕ l. KSub kϕ (kϕ :&* l)
  -- | Rename when going under an existing binder: don't touch that de Bruijn index!
  KUnder  :: forall kϕ kψ k. KSub kϕ kψ -> KSub (kϕ :&* k) (kψ :&* k)
  -- | Extend a substitution with a given type.
  KExtend :: KSub kϕ kψ -> Type kψ k -> KSub (kϕ :&* k) kψ
  -- | Compose two renamings.
  (:.:)   :: forall kϕ kψ kξ. KSub kψ kξ -> KSub kϕ kψ -> KSub kϕ kξ

-- | Apply a renaming/substitution to a type variable.
type SubVar :: KSub kϕ kψ -> KIdx kϕ k -> Type kψ k
type family SubVar s i where
  SubVar KId           i      = VarTy i             -- (Id)
  SubVar KBind         i      = VarTy (KS i)        -- (VarShift1)
  SubVar (KUnder _)    KZ     = VarTy KZ            -- (FVarLift1)
  SubVar (KUnder s)    (KS i) = Weaken (SubVar s i) -- (RVarLift1)
  SubVar (KExtend _ a) KZ     = a                   -- (FVarCons)
  SubVar (KExtend s _) (KS i) = SubVar s i          -- (RVarCons)

  -- Composition.
  SubVar (t :.: KId)              i      = SubVar t i                                      -- (IdL)
  SubVar (t :.: KBind)            i      = SubVar t (KS i)                                 -- (VarShift2)
  SubVar (t :.: KUnder _)         KZ     = SubVar t KZ                                     -- (FVarLift2)
  SubVar (t :.: KUnder s)         (KS i) = SubVar (t :.: KBind :.: s) i                    -- (RVarLift2)
  SubVar (t :.: ( KExtend s a ) ) i      = SubVar (KExtend ( t :.: s ) ( ApplySub t a )) i -- (MapEnv)
  SubVar (t :.: ( s :.: r ))      i      = SubVar (( t :.: s ) :.: r) i                    -- (AssEnv)

-- | Apply a renaming/substitution to a general type.
type ApplySub :: KSub kϕ kψ -> Type kϕ k -> Type kψ k
type family ApplySub s ty where
  ApplySub _ (LitTy @a)  = LitTy @a
  ApplySub s (VarTy i)   = SubVar s i
  ApplySub s (a :-> b)   = ApplySub s a :-> ApplySub s b
  ApplySub s (Forall ty) = Forall ( ApplySub ( KUnder s ) ty ) -- (Lambda)

-- | Rename when going under a forall.
type Weaken :: Type kϕ k -> Type (kϕ :&* l) k
type Weaken ty = ApplySub KBind ty

-- | Substitute a single type.
type SubOne :: Type kϕ k -> Type (kϕ :&* k) l -> Type kϕ l
type SubOne a b = ApplySub (KExtend KId a) b

----
-- Associated singletons.
----

applySubSing :: Sing s -> Sing a -> Sing (ApplySub s a)
applySubSing s = \case
  SLitTy     -> SLitTy
  SVarTy i   -> subVarSing s i
  a :%-> b   -> applySubSing s a :%-> applySubSing s b
  SForall ty -> SForall ( applySubSing ( SKUnder s ) ty )

subVarSing :: Sing s -> Sing i -> Sing (SubVar s i)
subVarSing SKId           i       = SVarTy i
subVarSing SKBind         i       = SVarTy (SKS i)
subVarSing (SKUnder _)    SKZ     = SVarTy SKZ
subVarSing (SKUnder s)    (SKS i) = weakenSing $ subVarSing s i
subVarSing (SKExtend _ a) SKZ     = a
subVarSing (SKExtend s _) (SKS i) = subVarSing s i
-- Composition.
subVarSing (t :%.: SKId)        i       = subVarSing t i
subVarSing (t :%.: SKBind)      i       = subVarSing t (SKS i)
subVarSing (t :%.: SKUnder _)   SKZ     = subVarSing t SKZ
subVarSing (t :%.: SKUnder s)   (SKS i) = subVarSing (t :%.: SKBind :%.: s) i
subVarSing (t :%.: (SKExtend s a)) i    = subVarSing ( SKExtend ( t :%.: s ) ( applySubSing t a )) i
subVarSing (t :%.: ( s :%.: r )) i      = subVarSing ( ( t :%.: s) :%.: r ) i

weakenSing :: Sing a -> Sing (Weaken a)
weakenSing = applySubSing SKBind

--------------------------------------------------------------------------------
-- Singletons.

data family Sing :: k -> Hs.Type

data instance Sing (k :: Kind) where
  SType :: Sing 'Type
  SFun  :: Sing a -> Sing b -> Sing (Fun a b)

infixr 0 :%->
data instance Sing (a :: Type kϕ k) where
  SLitTy  :: ( x ~ 'LitTy @a ) => Sing x
  (:%->)  :: Sing a -> Sing a -> Sing (a :-> b) 
  SVarTy  :: forall kϕ k (i :: KIdx kϕ k ). Sing i -> Sing (VarTy i)
  SForall :: Sing a -> Sing (Forall a)

data instance Sing (i :: KIdx kϕ k) where
  SKZ :: Sing KZ
  SKS :: Sing i -> Sing (KS i)

infixl 8 :%.:
data instance Sing (s :: KSub kϕ kψ) where
  SKId     :: Sing KId
  SKBind   :: Sing KBind
  SKUnder  :: Sing s -> Sing (KUnder s)
  SKExtend :: Sing k -> Sing s -> Sing (KExtend k s)
  (:%.:)   :: Sing s -> Sing t -> Sing (s :.: t)

--------------------------------------------------------------------------------
-- Displaying

deriving stock instance Show (Sing @Kind a)
deriving stock instance Show (Sing @(Type kϕ k) ty)
deriving stock instance Show (Sing @(KIdx kϕ k) idx)
deriving stock instance Show (Sing @(KSub kϕ kψ) sub)
deriving stock instance Show Kind
deriving stock instance Show KContext
deriving stock instance Show (KIdx kϕ k)
deriving stock instance Show (Type kϕ k)
