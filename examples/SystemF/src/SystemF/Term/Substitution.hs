{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators            #-}

{-# OPTIONS_GHC -fplugin=SystemF.Plugin #-}

module SystemF.Term.Substitution where

-- base
import qualified Data.Kind as Hs
import qualified Prelude   as Hs
import Prelude
  ( ($) )

-- system-f
import SystemF.Type
import SystemF.Term

--------------------------------------
-- Renaming and substitution of terms.

-- | A term renaming/substitution, which can be applied using 'applySub'.
type Sub :: KSub kϕ kψ -> Context kϕ -> Context kψ -> Hs.Type
data Sub s ϕ ψ where
  -- | Identity.
  Id      :: Sub KId ϕ ϕ
  -- | Rename in preparation for binding a new term variable.
  Bind    :: Sub KId ϕ (ϕ :& b)
  -- | Rename in preparation for binding a new type variable.
  BindT   :: Sub KBind ϕ (ϕ :*& k)
  -- | Rename when going under a term-level lambda abstraction.
  Under   :: Sub s ϕ ψ -> Sub s (ϕ :& b) (ψ :& ApplySub s b)
  -- | Rename when going under a big lambda.
  UnderT  :: Sub s ϕ ψ -> Sub (KUnder s) (ϕ :*& l) (ψ :*& l)
  -- | Extend a substitution with a given term.
  Extend  :: Sub s ϕ ψ -> Term ϕ a -> Sub s (ϕ :& a) ψ
  -- | Extend a substitution with a given type.
  ExtendT :: Sub s ϕ ψ -> Sing a -> Sub (KExtend s a) (ϕ :*& k) ψ

-- | Apply a renaming/substitution to a term variable.
subVar :: Sub s ϕ ψ -> Idx ϕ a -> Term ψ (ApplySub s a)
subVar Id          i = VarE i
subVar Bind        i = VarE $ S i
subVar BindT       i = VarE $ T i
subVar (Under  _ ) Z = VarE Z
subVar (Under  s) (S i) = weaken $ subVar s i
subVar (UnderT s) (T i) = weakenT $ subVar s i
subVar (Extend  s a) Z     = applySub s a
subVar (Extend  s _) (S i) = subVar s i
subVar (ExtendT s _) (T i) = subVar s i

-- | Apply a renaming/substitution to a term.
applySub :: Sub s ϕ ψ -> Term ϕ a -> Term ψ (ApplySub s a)
applySub s = \case
  LitE l    -> LitE l
  PrimE p   -> PrimE $ fmapPrimE ( applySub s ) p
  VarE i    -> subVar s i
  f :$ e    -> applySub s f :$ applySub s e
  LamE ty e -> LamE ( applySubSing ( subSing s ) ty ) ( applySub ( Under s ) e )
  f :@ ty   -> applySub s f :@ applySubSing ( subSing s ) ty
  TyLam ty  -> TyLam ( applySub ( UnderT s ) ty )

weaken :: Term ψ a -> Term (ψ :& b) a
weaken = applySub Bind

weakenT :: Term ϕ a -> Term (ϕ :*& k) (Weaken a)
weakenT = applySub BindT

subOne :: Term ϕ a -> Term (ϕ :& a) b -> Term ϕ b
subOne a b = applySub (Extend Id a) b

subOneT :: Sing a -> Term (ϕ :*& k) b -> Term ϕ (SubOne a b)
subOneT t b = applySub (ExtendT Id t) b

--------------------------------------------------------------------------------
-- Singletons.

subSing :: Sub s ϕ ψ -> Sing s
subSing Id            = SKId
subSing Bind          = SKId
subSing BindT         = SKBind
subSing (Under s)     = subSing s
subSing (UnderT s)    = SKUnder $ subSing s
subSing (Extend s _)  = subSing s
subSing (ExtendT s a) = SKExtend ( subSing s ) a

--------------------------------------------------------------------------------
-- Displaying

deriving stock instance Hs.Show (Sub s ϕ ψ)
