{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Fail5 where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add, Cancellable )

--------------------------------------------------------------------------------

-- This is like Fail1, except no rewriting is necessary to typecheck.
--
-- The plugin will nonetheless rewrite both type family applications,
-- emitting an extra "Cancellable b" constraint.
--
-- This shows that the rewriting occurs even when when no rewriting is necessary
-- for the type signature to typecheck.

fail5 :: Proxy ( Add 'Zero b ) -> Proxy ( Add 'Zero b )
fail5 px = px
