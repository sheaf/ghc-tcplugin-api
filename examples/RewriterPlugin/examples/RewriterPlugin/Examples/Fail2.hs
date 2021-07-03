{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Fail2 where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add )

--------------------------------------------------------------------------------

-- Plugin rewrites @ Add a z @ to @ a @,
-- but also emits a @ Cancellable a @ constraint.
--
-- Should get an error about an unsatisfied @ Cancellable a @ constraint.

fail2 :: ( z ~ 'Zero ) => Proxy z -> Proxy ( Add a z ) -> Proxy a
fail2 _ px = px
