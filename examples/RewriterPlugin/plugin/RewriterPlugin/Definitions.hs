{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module RewriterPlugin.Definitions where

-- base
import Data.Kind
  ( Constraint )

--------------------------------------------------------------------------------
-- Definitions that the plugin is interested in handling.

data Nat = Zero | Succ Nat | BadNat

type Add :: Nat -> Nat -> Nat
type family Add n m where {}

type Cancellable :: Nat -> Constraint
class Cancellable n where {}
