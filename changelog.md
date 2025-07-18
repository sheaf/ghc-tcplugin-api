
# Version 0.16.1.0 (2025-07-18)

- Correctness fix for the `GHC.TcPlugin.API.TyConSubst` module: ensure that
  the `TyConSubst` does not mistake representational Given equalities for
  nominal equalities. For the time being, the `splitTyConApp_upTo` functionality
  only works at nominal role.

# Version 0.16.0.0 (2025-07-18)

- Re-export `tyConDataCons`, `tyConSingleDataCon_maybe`,
  `tyConSingleDataCon`, `dataConTyCon`, `isNewTyCon` and `isNewDataCon`
  from GHC.

- The `isWanted` function now correctly returns `False` for derived
  constraints. This is only relevant for GHC 9.2 and below.

- Add `GHC.TcPlugin.API.TyConSubst` module, which implements logic for
  recognising when a type is a `TyConApp` up to Given constraints.

# Version 0.15.0.0 (2025-06-03)

- Remove `tcPluginIO` in favour of new `MonadIO` instance.  
  To migrate, use `liftIO` instead of `tcPluginIO`.

- Add `lookupTHName` function, which allows looking up Template Haskell names
  in typechecker plugins. Useful in conjunction with `TemplateHaskellQuotes`.

- Helper functions to construct evidence terms have been adjusted to return
  values of type `EvExpr`, rather than `EvTerm`. This makes the functions more
  composable, and allows typechecker plugin to provide evidence for quantified
  constraints more easily.

  Affected functions: `evDFunApp`, `evDataConApp`, `evCast`.

  To migrate, you will need to manually wrap evidence terms with the
  `EvExpr` constructor in places that expect an `EvTerm`.

- Added `natKind`, `symbolKind` and `charKind` for the kinds of type-level
  `Nat`, `Symbol` and `Char`.

- Added several re-exports from the `ghc` library:
  - `isGiven` & `isWanted`.
  - `ctEvPred`, `ctEvId`, `ctEvExpr` and `ctEvLoc`.
  - `className` & `tyConName`.
  - `isEqPred` (unboxed equality) and `isEqClassPred` (boxed equality).
  - `evId` & `ctEvId`.
  - `typeKind`.
  - `nonDetCmpType`.

- Removed re-exports of `ctev_pred`, `ctev_loc`, `ctev_evar`, and `ctev_dest`.

  Migration: use `ctEvPred` instead of `ctev_pred` and `ctEvLoc` instead of `ctev_loc`.
  Uses of `ctev_evar` and `ctev_dest` should be covered by `ctEvEvId` and/or
  `ctEvExpr`.

# Version 0.14.0.0 (2024-11-28)

- Rename `mkPrimEqPredRole` to `mkEqPredRole`. This is a re-exported function
  from GHC, and the renaming adapts to the renaming in GHC-9.13.

# Version 0.13.0.0 (2024-10-30)

- Update to changes in the type of GHC's `mkUnivCo`
  in order to (properly) add support for GHC 9.12.

- Change `mkPluginUnivCo`, `mkPluginUnivEvTerm` and `mkTyFamAppReduction`
  to take a `[Coercion]` rather than a `DVarSet` for specifying dependencies.

- Stop re-exporting `DVarSet`, `emptyDVarSet`, `extendDVarSet`, `unionDVarSet`,
  `unitDVarSet`, and `mkDVarSet`.

- Update documentation to suggest using `ctEvCoercion`
  rather than `ctEvId` to specify coercions that a `UnivCo` depends on.

- Re-export `ctEvCoercion`, and stop re-exporting `ctEvId`.

# Version 0.12.0.0 (2024-10-22)

- Add preliminary support for GHC 9.12.

- `mkPluginUnivCo`, `mkPluginUnivEvTerm` and `mkTyFamAppReduction` now all take
  an additional `DVarSet` argument which allows specifying evidence that we
  depend on. This stops evidence terms being floated out past used enclosing
  Givens (see [GHC issue #23923](https://gitlab.haskell.org/ghc/ghc/-/issues/23923)).

- Re-export `DVarSet`, `emptyDVarSet`, `extendDVarSet`, `unionDVarSet`,
  `unitDVarSet`, and `mkDVarSet`, as well as `ctEvId`, in order to facilitate
  construction and manipulation of `DVarSet`s.

- Re-export `GHC.Types.Unique.Set`, `GHC.Types.Unique.DSet`.

# Version 0.11.0.0 (2023-08-29)

- Add support for GHC 9.8.

- Re-export functionality relating to GHC's constraint solving `TcS` monad,
  such as `{get,set}InertSet`, `{get,set}TcEvBindsMap`.

- Re-export `readTcRef` and `writeTcRef`.

# Version 0.10.0.0 (2023-02-28)

- Introduce `resolveImport`, and make `PkgQual` opaque.

- Rename `tcRewriterWanteds` to `tcRewriterNewWanteds`
  (bringing it in line with nomenclature in GHC 9.4).

# Version 0.9.0.0 (2023-01-24)

- Add support for GHC 9.6 and `transformers` 0.6.

- The `One` and `Many` pattern synonyms are now `OneTy` and `ManyTy`.

- Use `mkInvisFunTy`/`mkInvisFunTys` instead of `mkInvisFunTyMany`/`mkInvisFunTysMany`.

# Version 0.8.3.0 (2022-10-05)

- Bugfix for the GHC 9.0 rewriter plugin compatibility shim:
  fix coercion orientations in family application cache

# Version 0.8.2.0 (2022-10-05)

- Bugfix for the GHC 9.0 rewriter plugin compatibility shim:
  fix coercion orientations in family rewriting.

# Version 0.8.1.0 (2022-10-05)

- Bugfix for the GHC 9.2 rewriter plugin compatibility shim:
  fix coercion orientations in family rewriting.

# Version 0.8.0.0 (2022-07-07)

- Compatibility for GHC 9.4.

- Change API for `lookupImportedModule` to use `PkgQual` and `UnitId`
  instead of `Maybe FastString`, with back-compatibility function `pkgQual_pkg`
  for use with older module lookup functions.

- Re-export `splitAppTys` and `unpackFS`.

# Version 0.7.1.0 (2022-01-04)

- `newWanted` now always uses the `CtLoc` information that it is provided with,
  as opposed to obtaining some information from the monadic environment.
  This means you no longer need to wrap calls to `newWanted` in `setCtLocM`
  to ensure that GHC reports the correct source span when reporting unsolved
  Wanteds in error messages.

- Remove the `newDerived` function, as Derived constraints are going to be
  removed from GHC.

# Version 0.7.0.0 (2021-12-31)

- Re-export functions for dealing with type-level literals,
  such as `mkNumLitTy` and `isStrLitTy`.

- Re-export functions for splitting apart type applications, such as
  `splitAppTy_maybe` and `tyConAppTyCon_maybe`.

- Redefine and re-export `mkUncheckedIntExpr` for GHC versions prior to 9.0.

- Re-export some basic types from `GHC.Types.Basic` such as `Arity`,
  `PromotionFlag` and `Boxity`.

- Re-export `GHC.Builtin.Names` and `GHC.Builin.Types.Prim`.

- Provide `MonadThings` instances for `TcPluginM` monads.

# Version 0.6.1.0 (2021-12-13)

- Re-export various useful types and functions to deal with type and coercion variables.

- Re-export a few types and functions to deal with source locations.

- Remove some re-exports for constructing function types, as not all functions make sense
  across all GHC versions supported by the library.

- Re-export `panic` and `pprPanic`.

# Version 0.6.0.0 (2021-12-13)

- Add support for GHC 8.8.

- Re-export `evDataConApp`, which is useful for constructing typeclass dictionaries.

# Version 0.5.1.0 (2021-08-31)

- Fix a bug in the type-family rewriting compatibility layer (GHC 8.10, 9.0, 9.2)
  by correctly downgrading the coercion used to cast the evidence, when necessary.

# Version 0.5.0.0 (2021-08-30)

- Re-export some additional types and functions that are useful for inspecting
  and constructing evidence terms, such as `mkTyVar`, `newName`, `mkLocalId`, `lookupEvBind`...

# Version 0.4.1.0 (2021-08-24)

- Re-export a few GHC modules, such as GHC.Core.Make and GHC.Plugins.
  These re-exports might be changed to be more selective in the future
  to aid cross-version compatibility.

# Version 0.4.0.0 (2021-08-24)

- Adapt to GHC 9.4 changes in the `TcPluginSolveResult` datatype:
  are now able to solve and emit constraints even when reporting
  a contradiction. This can help with error messages.
  Unfortunately these extra constraints will be dropped in versions
  of GHC prior to 9.4.

- Add a utility module for name resolution using constrained traversals.

- Add compatibility for GHC 8.10.

# Version 0.3.1.0 (2021-08-09)

Ensure that the coercions stored in `Reduction`s are always
oriented left-to-right, by making the internal rewriting compatibility layer
also use left-to-right coercions.

# Version 0.3.0.0 (2021-08-04)

Account for changes in rewriting in GHC 9.4:

  - rewriter plugins can no longer emit new Wanted constraints
    if they don't rewrite the type family application;
  - coercions in the rewriter are now oriented left-to-right,
    requiring `mkTyFamAppReduction` to be adapted.

# Version 0.2.0.0 (2021-07-22)

Initial release on Hackage.
