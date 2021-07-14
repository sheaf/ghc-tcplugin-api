{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module SystemF.Examples where

-- base
import qualified Prelude as Hs
import Prelude
  ( ($) )

-- system-f
import SystemF.Type
import SystemF.Term
import SystemF.Term.Evaluation

--------------------------------------------------------------------------------
-- Examples.

-- id :: forall a. a -> a
id :: Term Empty ( Forall ( VarTy KZ :-> VarTy KZ ) )
id = TyLam $ LamE ( SVarTy SKZ ) ( VarE Z )

-- Specialise 'id' to integers.
-- id2 :: Int -> Int
-- id2 = id @Int
idAtInt :: Term Empty ( Int :-> Int )
idAtInt = id :@ SLitTy @Int

-- Apply 'id' to the integer '17'.
idAtInt17 :: Term Empty Int
idAtInt17 = idAtInt :$ LitE (IntLit 17)

square :: Term Empty (Int :-> Int)
square = LamE ( SLitTy @Int ) $ PrimE ( Mul ( VarE Z ) ( VarE Z ) )

--------------------------
-- Church numerals.

type ChurchKind :: Kind -> Kind
type ChurchKind k = Fun (Fun k k) (Fun k k)

type ChurchType :: forall (k :: Kind). Type KEmpty (ChurchKind k)
type ChurchType = Forall ( (VarTy KZ :-> VarTy KZ) :-> (VarTy KZ :-> VarTy KZ) )

church :: forall (k :: Kind). Hs.Int -> Term Empty (ChurchType @k)
church i = TyLam $ LamE ( SVarTy SKZ :%-> SVarTy SKZ ) $ LamE ( SVarTy SKZ ) ( go i )
  where
    go :: Hs.Int -> Term ( Empty :*& k :& (VarTy KZ :-> VarTy KZ) :& VarTy KZ ) ( VarTy KZ )
    go 0 = VarE Z
    go j = VarE (S Z) :$ go ( j Hs.- 1 )

evalChurchSquare :: Hs.Int -> Hs.Int -> Hs.Int
evalChurchSquare i arg = case eval (church i :@ SLitTy @Int :$ square :$ LitE (IntLit arg) ) of
  LitV (IntLit res) -> res
