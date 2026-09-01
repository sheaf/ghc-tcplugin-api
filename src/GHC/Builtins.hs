{-# LANGUAGE CPP #-}

-- | Helper umbrella re-exports of various known entities to GHC: known-key
-- uniques, wired-in types.
module GHC.Builtins
  ( -- * Known-key uniques
    hasKey
  , eqTyConKey, heqTyConKey, coercibleTyConKey
  , eqPrimTyConKey, eqReprPrimTyConKey, eqPhantPrimTyConKey

    -- * Custom type errors (prefer using 'GHC.TcPlugin.API.mkTcPluginErrorTy')
#if MIN_VERSION_ghc(10,1,0)
  , errorMessageTypeErrorFamKey
  , typeErrorTextDataConKey, typeErrorShowTypeDataConKey
  , typeErrorAppendDataConKey, typeErrorVAppendDataConKey
#else
  , errorMessageTypeErrorFamName
  , typeErrorTextDataConName, typeErrorShowTypeDataConName
  , typeErrorAppendDataConName, typeErrorVAppendDataConName
#endif

    -- * Equality and coercions
  , eqTyCon, heqTyCon, coercibleTyCon
  , eqPrimTyCon, eqReprPrimTyCon, eqPhantPrimTyCon
  , eqClass, heqClass, coercibleClass
  , eqDataCon, heqDataCon, coercibleDataCon
#if MIN_VERSION_ghc(8,10,0)
  , equalityTyCon
#endif

    -- * Kinds
  , liftedTypeKind, constraintKind, typeSymbolKind
  , unboxedTupleKind, unboxedSumKind
  , liftedTypeKindTyCon, constraintKindTyCon, tYPETyCon
  , anyTy, anyTyCon, anyTypeOfKind
#if MIN_VERSION_ghc(8,10,0)
  , typeToTypeKind
#endif
#if MIN_VERSION_ghc(9,1,0)
  , unliftedTypeKind
#endif
#if MIN_VERSION_ghc(9,3,0)
  , zeroBitTypeKind
#endif
#if MIN_VERSION_ghc(9,5,0)
  , tYPEKind, cONSTRAINTKind, cONSTRAINTTyCon, fUNTyCon
#endif

    -- * Runtime representations, levities and multiplicities
  , runtimeRepTy, runtimeRepTyCon, liftedRepTy
#if MIN_VERSION_ghc(9,0,0)
  , multiplicityTy, multiplicityTyCon
  , manyDataCon, oneDataCon, manyDataConTy, oneDataConTy
#endif
#if MIN_VERSION_ghc(9,1,0)
  , unliftedRepTy, boxedRepDataConTyCon, levityTyCon
  , liftedDataConTy, unliftedDataConTy
  , liftedDataConTyCon, unliftedDataConTyCon
#endif
#if MIN_VERSION_ghc(9,3,0)
  , levityTy, zeroBitRepTy
#endif

    -- * Wired-in types
  , unitTy, unitTyCon, unitDataCon, unitDataConId
  , boolTy, boolTyCon, trueDataCon, falseDataCon
  , promotedTrueDataCon, promotedFalseDataCon
  , orderingTyCon, promotedLTDataCon, promotedEQDataCon, promotedGTDataCon
  , charTy, charTyCon, charDataCon
  , intTy, intTyCon, intDataCon
  , wordTy, wordTyCon, wordDataCon
  , floatTy, floatTyCon, doubleTy, doubleTyCon
  , stringTy
  , listTyCon, nilDataCon, consDataCon
  , promotedNilDataCon, promotedConsDataCon
  , maybeTyCon, justDataCon, nothingDataCon
  , promotedJustDataCon, promotedNothingDataCon
#if MIN_VERSION_ghc(9,0,0)
  , integerTy, integerTyCon, naturalTy, naturalTyCon
#endif
#if !MIN_VERSION_ghc(9,1,0)
  , typeNatKind
#endif

    -- * Tuples and sums
  , tupleTyCon, tupleDataCon, promotedTupleDataCon
  , sumTyCon, sumDataCon
  , cTupleTyConName, isCTupleTyConName
  , mkTupleTy, mkBoxedTupleTy, mkSumTy
  , mkListTy, mkPromotedListTy
#if MIN_VERSION_ghc(8,10,0)
  , mkTupleTy1
#endif
#if MIN_VERSION_ghc(9,1,0)
  , cTupleTyCon, cTupleDataCon
  , mkMaybeTy, mkPromotedMaybeTy, mkPromotedPairTy
#endif
#if MIN_VERSION_ghc(9,5,0)
  , mkConstraintTupleTy
#endif

    -- * Type-level literals
  , typeNatTyCons, typeNatCoAxiomRules
  , typeNatAddTyCon, typeNatSubTyCon, typeNatMulTyCon, typeNatExpTyCon
  , typeNatDivTyCon, typeNatModTyCon, typeNatLogTyCon, typeNatCmpTyCon
  , typeSymbolCmpTyCon, typeSymbolAppendTyCon, typeSymbolKindCon
#if MIN_VERSION_ghc(9,1,0)
  , typeConsSymbolTyCon, typeUnconsSymbolTyCon
  , typeCharCmpTyCon, typeCharToNatTyCon, typeNatToCharTyCon
#endif

    -- * Primitive types
  , intPrimTy, wordPrimTy, charPrimTy, floatPrimTy, doublePrimTy
  , realWorldTy, realWorldTyCon, realWorldStatePrimTy
  , mkStatePrimTy, mkProxyPrimTy, proxyPrimTyCon
  )
  where

-- ghc
#if MIN_VERSION_ghc(10,1,0)
import GHC.Builtin.KnownKeys
import GHC.Builtin.WiredIn.Prim
import GHC.Builtin.WiredIn.TypeLits
import GHC.Builtin.WiredIn.Types
#else
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Literals
import GHC.Builtin.Types.Prim
#endif
