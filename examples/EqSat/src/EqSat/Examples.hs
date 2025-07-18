{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=EqSat.Plugin #-}
{-# OPTIONS_GHC -dcore-lint #-}

module EqSat.Examples where

-- base
import Data.Proxy
import GHC.TypeNats

--------------------------------------------------------------------------------
-- Examples.

ex1 :: Proxy ( n + m ) -> Proxy ( m + n )
ex1 x = x

ex2 :: ( u + 1 ~ n + m ) => Proxy ( u + 2 ) -> Proxy ( m + n + 1 )
ex2 x = x

ex3 :: Proxy ( a + ( b + c ) ) -> Proxy ( ( a + b ) + c )
ex3 x = x
