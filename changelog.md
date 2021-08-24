
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
