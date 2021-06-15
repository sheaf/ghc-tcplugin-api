{-# OPTIONS_GHC -fplugin=RewriterPlugin #-}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module RewriterPlugin.Examples.Pass where

-- base
import Data.Proxy
  ( Proxy )

-- RewriterPlugin
import RewriterPlugin.Definitions
  ( Nat(..), Add, Cancellable )

--------------------------------------------------------------------------------

pass1 :: Cancellable b => Proxy ( Add 'Zero b ) -> Proxy b
pass1 px = px

pass2 :: Cancellable a => Proxy ( Add a 'Zero ) -> Proxy a
pass2 px = px

-- this case requires GHC to have rewritten the type family arguments
-- before asking the plugin for a rewriting
pass3 :: ( z ~ 'Zero, Cancellable a ) => Proxy z -> Proxy ( Add a z ) -> Proxy a
pass3 _ px = px
