{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=SystemF.Plugin #-}

module SystemF.Plugin.Test where

-- base
import Data.Proxy
  ( Proxy )

-- system-f
import SystemF.Type

--------------------------------------------------------------------------------
-- Plugin tests.

clos :: Proxy t -> Proxy s -> Proxy a
     -> Proxy ( ApplySub t ( ApplySub s a ) ) 
     -> Proxy ( ApplySub ( t :*: s ) a )
clos _ _ _ x = x

assEnv :: Proxy t -> Proxy s -> Proxy r -> Proxy a
       -> Proxy ( ApplySub ( t :*: ( s :*: r ) ) a )
       -> Proxy ( ApplySub ( ( t :*: s ) :*: r ) a )
assEnv _ _ _ _ x = x

mapEnv :: Proxy t -> Proxy s -> Proxy a -> Proxy b
       -> Proxy ( ApplySub ( t :*: KExtend s a ) b )
       -> Proxy ( ApplySub ( KExtend ( t :*: s ) ( ApplySub t a ) ) b )
mapEnv _ _ _ _ x = x

shiftCons :: Proxy s -> Proxy a -> Proxy b
          -> Proxy ( ApplySub ( KExtend s a :*: KBind ) b )
          -> Proxy ( ApplySub s b )
shiftCons _ _ _ x = x

shiftLift1 :: Proxy s -> Proxy l -> Proxy a
           -> Proxy ( ApplySub ( KUnder s :*: KBind @l ) a )
           -> Proxy ( ApplySub ( KBind @l :*: s ) a )
shiftLift1 _ _ _ x = x

shiftLift2 :: Proxy t -> Proxy s -> Proxy l -> Proxy a
           -> Proxy ( ApplySub ( ( t :*: KUnder s ) :*: KBind @l ) a )
           -> Proxy ( ApplySub ( ( t :*: KBind @l ) :*: s ) a )
shiftLift2 _ _ _ _ x = x

lift1 :: Proxy t -> Proxy s -> Proxy a
      -> Proxy ( ApplySub ( KUnder t :*: KUnder s ) a )
      -> Proxy ( ApplySub ( KUnder ( t :*: s ) ) a )
lift1 _ _ _ x = x

lift2 :: Proxy u -> Proxy t -> Proxy s -> Proxy a
      -> Proxy ( ApplySub ( ( u :*: KUnder t ) :*: KUnder s ) a )
      -> Proxy ( ApplySub ( u :*: KUnder ( t :*: s ) ) a )
lift2 _ _ _ _ x = x

liftEnv :: Proxy t -> Proxy a -> Proxy s -> Proxy b
        -> Proxy ( ApplySub ( KExtend t a :*: KUnder s ) b )
        -> Proxy ( ApplySub ( KExtend ( t :*: s ) a ) b )
liftEnv _ _ _ _ x = x

--------------------------------------------------------------------------------

ids :: Proxy s1 -> Proxy s2 -> Proxy s3 -> Proxy b
    -> Proxy ( ApplySub ( s1 :*: KUnder ( s2 :*: KUnder ( KId :*: KUnder KId ) ) :*: s3 ) b )
    -> Proxy ( ApplySub ( s1 :*: KUnder s2 :*: s3 ) b )
ids _ _ _ _ x = x

reassoc :: Proxy s1 -> Proxy s2 -> Proxy s3
        -> Proxy s4 -> Proxy s5 -> Proxy s6
        -> Proxy a
        -> Proxy ( ApplySub ( s1 :*: ( ( s2 :*: s3 ) :*: ( s4 :*: ( s5 :*: s6 ) ) ) ) a )
        -> Proxy ( ApplySub ( s1 :*: s2 :*: s3 :*: s4 :*: s5 :*: s6 ) a )
reassoc _ _ _ _ _ _ _ x = x