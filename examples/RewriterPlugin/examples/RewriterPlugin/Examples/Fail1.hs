{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Fail1 where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add )

--------------------------------------------------------------------------------

-- Plugin rewrites @ Add 'Zero b @ to @ b @,
-- but also emits a @ Cancellable b @ constraint.
--
-- Should get an error about an unsatisfied @ Cancellable b @ constraint.

fail1 :: Proxy ( Add 'Zero b ) -> Proxy b
fail1 px = px
