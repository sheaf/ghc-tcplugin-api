{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module SystemF.Term.Evaluation where

-- base
import qualified Data.Kind as Hs
import qualified Prelude   as Hs
import Prelude
  ( Maybe(..), ($), (<$>) )

-- system-f
import SystemF.Term
import SystemF.Term.Substitution
import SystemF.Type

--------------------------------------------------------------------------------
-- Reduction and evalaution of terms.

type Val :: Context kϕ -> Type kϕ k -> Hs.Type
data Val ϕ a where
  LitV   :: !(LitE a) -> Val ϕ (LitTy @a)
  LamV   :: Sing a -> Term (ϕ :& a) b -> Val ϕ (a :-> b)
  TyLamV :: Term (ϕ :*& k) b -> Val ϕ (Forall b)

deriving stock instance Hs.Show (Val kϕ k)

step :: Term Empty a -> Maybe ( Term Empty a )
step = \case
  LitE  {} -> Nothing
  PrimE {} -> Nothing
  LamE  {} -> Nothing
  TyLam {} -> Nothing
  f :$ a   -> stepApp   a f
  f :@ t   -> stepTyApp t f

stepApp :: Term Empty a -> Term Empty (a :-> b) -> Maybe (Term Empty b)
stepApp a = \case
  g :$ b   -> (:$ a) <$> stepApp   b g 
  g :@ s   -> (:$ a) <$> stepTyApp s g 
  LamE _ b -> Just $ subOne a b

stepTyApp :: Sing a -> Term Empty (Forall ty) -> Maybe (Term Empty (SubOne a ty))
stepTyApp t = \case
  g :$ b  -> (:@ t) <$> stepApp   b g 
  g :@ s  -> (:@ t) <$> stepTyApp s g 
  TyLam s -> Just $ subOneT t s

eval :: Term Empty a -> Val Empty a
eval (LitE l)   = LitV l
eval (TyLam e)  = TyLamV e
eval (LamE t f) = LamV t f
eval (PrimE p)  = evalPrim $ fmapPrimE eval p
eval (f :$ a)   = case eval f of { LamV _ g -> eval $ subOne  a g }
eval (f :@ t)   = case eval f of { TyLamV g -> eval $ subOneT t g }

evalPrim :: PrimE Val Empty a -> Val Empty a
evalPrim ( Add x y )
  | LitV ( IntLit a ) <- x
  , LitV ( IntLit b ) <- y
  = LitV ( IntLit $ a Hs.+ b )
evalPrim ( Mul x y )
  | LitV ( IntLit a ) <- x
  , LitV ( IntLit b ) <- y
  = LitV ( IntLit $ a Hs.* b )
evalPrim ( Concat x y )
  | LitV ( StringLit a ) <- x
  , LitV ( StringLit b ) <- y
  = LitV ( StringLit $ a Hs.<> b )
evalPrim ( Length x )
  | LitV ( StringLit a ) <- x
  = LitV ( IntLit $ Hs.length a )

reduce :: Term ϕ a -> Term ϕ a
reduce (LitE l) = LitE l
reduce (VarE i) = VarE i
reduce (LamE t a) = LamE t ( reduce a )
reduce (TyLam e) = TyLam ( reduce e )
reduce (PrimE p) = PrimE $ fmapPrimE reduce p
reduce (f :$ a) = case reduce f of
  LamE _ b -> subOne (reduce a) b
  b        -> b :$ reduce a
reduce (f :@ t) = case reduce f of
  TyLam g -> subOneT t g
  g       -> g :@ t
