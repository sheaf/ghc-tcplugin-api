{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Fail4 where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add )

--------------------------------------------------------------------------------

-- Plugin should throw an error about the 'BadNat.
-- The error should not be duplicated.

fail4 :: Proxy ( Add a 'BadNat ) -> Proxy a
fail4 px = px
