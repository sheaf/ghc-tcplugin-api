
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
