{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module EqSat.Plugin where

-- base
import Control.Applicative
  ( (<|>) )
import Control.Monad
  ( void )
import Data.Foldable
  ( for_ )
import Data.Function
  ( (&) )
import Data.Maybe
  ( mapMaybe )

-- containers
import qualified Data.Set as Set
  ( filter )

-- ghc
import GHC.Builtin.Types.Literals
  ( typeNatAddTyCon, typeNatMulTyCon, typeNatSubTyCon )
import GHC.Core.TyCo.Rep
  ( Coercion, TyLit(..), Type(..)
  , nonDetCmpTyLit
  )
import GHC.Core.TyCon
  ( TyCon )
import GHC.Types.Name
  ( Name )
import GHC.Types.Unique
  ( nonDetCmpUnique, getUnique )
import GHC.Types.Var
  ( ForAllTyBinder, FunTyFlag, Var )
import GHC.Utils.Monad
  ( mapMaybeM )
import Language.Haskell.Syntax.Specificity
  ( ForAllTyFlag )

-- hegg
import Data.Equality.Analysis
  ( Analysis(..) )
import Data.Equality.Graph
  ( EGraph )
import qualified Data.Equality.Graph as EG
  ( find )
import qualified Data.Equality.Graph as EG.Plain
import qualified Data.Equality.Graph.Monad as EG
  ( egraph, merge, represent, rebuild )
import Data.Equality.Language
  ( Language )
import Data.Equality.Matching.Pattern
  ( Pattern(..) )
import Data.Equality.Saturation
  ( Fix(..), Rewrite(..) )
import qualified Data.Equality.Saturation as EG
  ( runEqualitySaturation )
import qualified Data.Equality.Saturation.Scheduler as EG
  ( defaultBackoffScheduler )
import Data.Equality.Graph.Lens

-- ghc
import qualified GHC.Plugins as GHC
  ( Plugin(..), defaultPlugin, purePlugin )
import GHC.Utils.Outputable
  ( (<+>), vcat, text, showPprUnsafe )

-- ghc-tcplugin-api
import GHC.TcPlugin.API

--------------------------------------------------------------------------------

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.tcPlugin        = \ _args -> Just $ mkTcPlugin tcPlugin
    , GHC.pluginRecompile = GHC.purePlugin
    }

tcPlugin :: TcPlugin
tcPlugin =
  TcPlugin
    { tcPluginInit    = return ()
    , tcPluginSolve   = pluginSolve
    , tcPluginRewrite = \ _ -> emptyUFM
    , tcPluginStop    = \ _ -> return ()
    }

pluginSolve :: () -> [ Ct ] -> [ Ct ] -> TcPluginM Solve TcPluginSolveResult
pluginSolve _ _ [] = return $ TcPluginOk [] []
pluginSolve _ givens wanteds = do

  tcPluginTrace "eq-sat" $
    vcat
      [ text "givens:" <+> ppr givens
      , text "wanteds:" <+> ppr wanteds
      ]

  let
    eg :: EGraph ( Maybe ETyLit ) EType
    ( wantedIds, eg ) = EG.egraph do

      -- Add Given constraints
      givenIds <-
        ( `mapMaybeM` givens ) \ g ->
          case classifyPredType ( ctPred g ) of
            EqPred NomEq lhs rhs -> do
              a <- EG.represent $ toEType lhs
              b <- EG.represent $ toEType rhs
              i <- EG.merge a b
              return $ Just ( g, i )
            _ ->
              return Nothing

      -- Add Wanted constraints
      wantedsWithIds <-
        ( `mapMaybeM` wanteds ) \ w ->
          case classifyPredType ( ctPred w ) of
            EqPred NomEq lhs rhs -> do
              a <- EG.represent $ toEType lhs
              b <- EG.represent $ toEType rhs
              return $ Just ( w, ( ( lhs, a ), ( rhs, b ) ) )
            _ ->
              return Nothing

      -- Run equality saturation
      EG.runEqualitySaturation EG.defaultBackoffScheduler rewrites

      return wantedsWithIds

    solvedWanteds =
      ( `mapMaybe` wantedIds ) \ ( w, ( ( lhs, a ), ( rhs, b ) ) ) ->
        if EG.find a eg == EG.find b eg
        then
          let ev = mkPluginUnivEvTerm "eq-sat" Nominal [] lhs rhs
            -- TODO: declare if we used any Givens
          in Just ( ev, w )
        else Nothing

  tcPluginTrace "eq-sat" $
    vcat
      [ text "givens:" <+> ppr givens
      , text "wanteds:" <+> ppr wanteds
      , text "solved wanteds:" <+> ppr solvedWanteds
      , text "e-graph:" <+> text (show eg)
      ] --  No instance for `Show (EType EG.Plain.ClassId)'

  return $ TcPluginOk solvedWanteds []

--------------------------------------------------------------------------------


newtype ETyCon = ETyCon TyCon
  deriving newtype Eq
instance Ord ETyCon where
  compare ( ETyCon tc1 ) ( ETyCon tc2 ) =
    getUnique tc1 `nonDetCmpUnique` getUnique tc2
instance Show ETyCon where
  show ( ETyCon tc ) = showPprUnsafe tc

newtype ETyLit = ETyLit TyLit
  deriving newtype Eq
instance Ord ETyLit where
  compare ( ETyLit lit1 ) ( ETyLit lit2 ) =
    nonDetCmpTyLit lit1 lit2
instance Show ETyLit where
  show ( ETyLit lit ) = showPprUnsafe lit

newtype ECoercion = ECoercion Coercion
instance Eq ECoercion where
  _ == _ = True
instance Ord ECoercion where
  compare _ _ = EQ
instance Show ECoercion where
  show ( ECoercion co ) = showPprUnsafe co

-- TODO: orphans
instance Show ForAllTyBinder where
  show = showPprUnsafe
instance Show FunTyFlag where
  show = showPprUnsafe
instance Show Var where
  show = showPprUnsafe

data EType ty
  = ETyVarTy Var
  | EAppTy ty ty
  | ETyConApp ETyCon [ty]
  | EForAllTy ForAllTyBinder ty
  | EFunTy FunTyFlag ty ty ty
  | ELitTy ETyLit
  | ECastTy ty ECoercion
  | ECoercionTy ECoercion
  deriving stock ( Eq, Ord, Show, Functor, Foldable, Traversable )
  -- TODO: Eq/Ord instances possibly a bit dodgy,
  -- with e.g. metavariables as well as ForAllTy

toEType :: Type -> Fix EType
toEType = Fix . \case
  TyVarTy tv -> ETyVarTy tv
  AppTy f a -> EAppTy ( toEType f ) ( toEType a )
  TyConApp tc args -> ETyConApp ( ETyCon tc ) ( map toEType args )
  ForAllTy tv body -> EForAllTy tv ( toEType body )
  FunTy ftf mult arg res -> EFunTy ftf ( toEType mult ) ( toEType arg ) ( toEType res )
  LitTy lit -> ELitTy ( ETyLit lit )
  CastTy ty co -> ECastTy ( toEType ty ) ( ECoercion co )
  CoercionTy co -> ECoercionTy ( ECoercion co )

fromEType :: Fix EType -> Type
fromEType ( Fix e ) =
  case e of
    ETyVarTy tv -> TyVarTy tv
    EAppTy f a -> AppTy ( fromEType f ) ( fromEType a )
    ETyConApp ( ETyCon tc ) args -> TyConApp tc $ map fromEType args
    EForAllTy tv body -> ForAllTy tv ( fromEType body )
    EFunTy ftf mult arg res -> FunTy ftf ( fromEType mult ) ( fromEType arg ) ( fromEType res )
    ELitTy ( ETyLit lit ) -> LitTy lit
    ECastTy ty ( ECoercion co ) -> CastTy ( fromEType ty ) co
    ECoercionTy ( ECoercion co ) -> CoercionTy co

instance Analysis ( Maybe ETyLit ) EType where

  makeA = \case
    ELitTy lit -> Just lit

    ETyConApp ( ETyCon tc ) args
      | tc == typeNatAddTyCon
      , [ Just ( ETyLit ( NumTyLit a ) ), Just ( ETyLit ( NumTyLit b ) ) ] <- args
      -> Just $ ETyLit $ NumTyLit $ a + b
      | tc == typeNatSubTyCon
      , [ Just ( ETyLit ( NumTyLit a ) ), Just ( ETyLit ( NumTyLit b ) ) ] <- args
      -> Just $ ETyLit $ NumTyLit $ a - b
      | tc == typeNatMulTyCon
      , [ Just ( ETyLit ( NumTyLit a ) ), Just ( ETyLit ( NumTyLit b ) ) ] <- args
      -> Just $ ETyLit $ NumTyLit $ a * b
      | otherwise
      -> Nothing

    ETyVarTy {} -> Nothing
    EAppTy {} -> Nothing
    EForAllTy {} -> Nothing
    EFunTy {} -> Nothing
    ECastTy {} -> Nothing
    ECoercionTy {} -> Nothing

  joinA = (<|>)

  modifyA cl eg0 =
    case eg0^._class cl._data of
      Nothing -> eg0
      Just d  ->
            -- Add constant as e-node
        let (new_c,eg1) = EG.Plain.represent (Fix $ ELitTy d) eg0
            (rep, eg2)  = EG.Plain.merge cl new_c eg1
            -- Prune all except leaf e-nodes
         in eg2 & _class rep._nodes %~ Set.filter ( null . EG.Plain.unNode )
              -- TODO: pruning could potentially prevent us from seeing some equalities?
              -- Rodrigo: "I think it's fine"



-- TODO: use TemplateHaskellQuotes to provide an API for writing these rewrite rules
-- without having to manually write the constructors

rewrites :: [ Rewrite a EType ]
rewrites =
  [ -- a + b = b + a
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ "a", "b" ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ "b", "a" ] )
  ,

    -- a + 0 = a
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ "a", NonVariablePattern $ ELitTy $ ETyLit $ NumTyLit 0 ] )
     :=
    "a"
  ,

    -- a + ( b + c ) = ( a + b ) + c
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ "a", NonVariablePattern $ ETyConApp ( ETyCon typeNatAddTyCon ) [ "b", "c" ] ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ NonVariablePattern $ ETyConApp ( ETyCon typeNatAddTyCon ) [ "a", "b" ], "c" ] )
  ,

    -- a * b = b * a
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "b" ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "b", "a" ] )
  ,

    -- a * 1 = a
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", NonVariablePattern ( ELitTy ( ETyLit $ NumTyLit 1 ) ) ] )
     :=
    "a"
  ,

    -- a * 0 = 0
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", NonVariablePattern ( ELitTy ( ETyLit $ NumTyLit 0 ) ) ] )
     :=
    NonVariablePattern ( ELitTy ( ETyLit $ NumTyLit 0 ) )
  ,

    -- a * ( b * c ) = ( a * b ) * c
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "b", "c" ] ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "b" ], "c" ] )
  ,

    -- a * ( b + c ) = a * b + a * c
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", NonVariablePattern $ ETyConApp ( ETyCon typeNatAddTyCon ) [ "b", "c" ] ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatAddTyCon ) [ NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "b" ], NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "c" ] ] )
  ,

    -- a - a = 0
    NonVariablePattern ( ETyConApp ( ETyCon typeNatSubTyCon ) [ "a", "a" ] )
     :=
    NonVariablePattern ( ELitTy ( ETyLit $ NumTyLit 0 ) )
  ,

    -- a * ( b - c ) = a * b - a * c
    NonVariablePattern ( ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", NonVariablePattern $ ETyConApp ( ETyCon typeNatSubTyCon ) [ "b", "c" ] ] )
     :=
    NonVariablePattern ( ETyConApp ( ETyCon typeNatSubTyCon ) [ NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "b" ], NonVariablePattern $ ETyConApp ( ETyCon typeNatMulTyCon ) [ "a", "c" ] ] )

  ]
