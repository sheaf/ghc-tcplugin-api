{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module SystemF.Term where

-- base
import qualified Data.Kind as Hs
import qualified Prelude   as Hs

-- system-f
import SystemF.Type
  ( Kind(..), KContext(..), Type(..)
  , String, Int
  , SubOne, Weaken
  , Sing(..)
  )

--------------------------------------------------------------------------------
-- Terms.

infixl 4 :&
infixl 5 :*&
-- | Term-level context.
type Context :: KContext -> Hs.Type
data Context kϕ where
  Empty      :: Context KEmpty
  -- | Extend a context by a kind (use the synonym '(:*&)' instead).
  KExtendCtx :: forall k kϕ. Context kϕ -> Context (kϕ :&* k)
  -- | Extend a context by a type.
  (:&)       :: Context kϕ -> Type kϕ k -> Context kϕ

-- | Extend a context by a kind.
type (:*&) :: forall (kϕ :: KContext). Context kϕ -> forall (k :: Kind) -> Context (kϕ :&* k)
type ϕ :*& k = KExtendCtx @k ϕ

-- | Term-level de Bruijn index.
type Idx :: Context kϕ -> Type kϕ k -> Hs.Type
data Idx ϕ a where
  Z :: Idx (ϕ :& a) a
  S :: Idx ϕ a -> Idx (ϕ :&  b) a
  T :: Idx ϕ a -> Idx (ϕ :*& l) (Weaken a)

-- | Literal expression.
type LitE :: Hs.Type -> Hs.Type
data LitE t where
  IntLit    :: !Hs.Int    -> LitE Hs.Int
  StringLit :: !Hs.String -> LitE Hs.String

-- | Primitive operations.
type PrimE :: ( Context kϕ -> Type kϕ a -> Hs.Type ) -> Context kϕ -> Type kϕ a -> Hs.Type
data PrimE f ϕ a where
  Add    :: f ϕ Int -> f ϕ Int -> PrimE f ϕ Int
  Mul    :: f ϕ Int -> f ϕ Int -> PrimE f ϕ Int
  Concat :: f ϕ String -> f ϕ String -> PrimE f ϕ String
  Length :: f ϕ String -> PrimE f ϕ Int

fmapPrimE :: ( forall l. f ϕ (LitTy @l) -> g ψ (LitTy @l) )
          -> PrimE f ϕ (LitTy @a)
          -> PrimE g ψ (LitTy @a)
fmapPrimE f ( Add a b ) = Add ( f a ) ( f b )
fmapPrimE f ( Mul a b ) = Mul ( f a ) ( f b )
fmapPrimE f ( Concat a b ) = Concat ( f a ) ( f b )
fmapPrimE f ( Length a ) = Length ( f a )

infixl 9 :$
infixl 9 :@
-- | A term, in the given context, of the given type.
type Term :: Context kϕ -> Type kϕ k -> Hs.Type
data Term ϕ a where
  LitE  :: !(LitE a) -> Term ϕ (LitTy @a)
  PrimE :: !(PrimE Term ϕ (LitTy @a)) -> Term ϕ (LitTy @a)
  LamE  :: Sing a -> Term (ϕ :& a) b -> Term ϕ (a :-> b)
  TyLam :: Term (ϕ :*& k) b -> Term ϕ (Forall b)
  (:$)  :: Term ϕ (a :-> b) -> Term ϕ a -> Term ϕ b
  (:@)  :: Term ϕ (Forall ty) -> Sing a -> Term ϕ (SubOne a ty)
  VarE  :: !(Idx ϕ a) -> Term ϕ a

--------------------------------------------------------------------------------
-- Displaying

deriving stock instance Hs.Show (Context kϕ)
deriving stock instance Hs.Show (Idx ϕ a)
deriving stock instance Hs.Show (LitE a)
deriving stock instance ( forall x. Hs.Show (Sing x) => Hs.Show (t ϕ x) ) => Hs.Show (PrimE t ϕ a)
deriving stock instance Hs.Show (Term ϕ a)
