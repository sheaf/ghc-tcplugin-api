{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Fail3 where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add, Cancellable )

--------------------------------------------------------------------------------

-- Plugin should not be able to rewrite @ Add a z @ to @ a @,
-- so we expect a mismatch between those two types.

fail3 :: Cancellable a => Proxy z -> Proxy ( Add a z ) -> Proxy a
fail3 _ px = px
