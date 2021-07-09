

# ghc-tcplugin-api

This library attempts to provide a convenient interface for authors of GHC type-checking plugins.

Different stages of a type-checking plugin (initialisation, solving, rewriting, shutdown) are given
different monads to operate within. This ensures operations that only make sense in one context
aren't mistakenly carried out in another.    

To provide a unified interface to these monads (whenever this makes sense), two MTL-style typeclasses
are provided: `MonadTcPlugin` and `MonadTcPluginTypeError`. The first enables overloading of
monadic operations common to all stages, and the second allows plugins to throw custom type-errors,
but only in the solving or rewriting phases.   
`MonadTcPlugin` is internally implemented using lifting and unlifting of GHC's `TcM` monad, but it
is hoped that users will not need to access these internals. 

This library provides functionality for throwing custom type-errors, by means of the datatype
`TcPluginErrorMessage` (which mimics the `ErrorMessage` datakind from `GHC.TypeLits`) and the
interpreter `mkTcPluginErrorTy`.    

One goal of this library is to ensure that authors of type-checking plugins should, for the most part,
only need to import this library for their type-checking plugin needs, and not the `ghc` package itself.    
This should assist discoverability: instead of having to trawl through the entire GHC codebase,
new users should be able to determine at a glance what functions might be appropriate for their
usecase using this library's interface. This can be particularly useful when using typed holes.

## Compatibility

This library makes use of GHC's new API for rewriting type-families in typechecker plugins,
which is scheduled to land in GHC 9.4.    
A compatibility layer is provided, which retro-fits the rewriting functionality onto GHC 9.0 and 9.2.    

## Documentation

The haddocks for this library is available online [here](https://sheaf.github.io/ghc-tcplugin-api/).

## Examples

A simple example plugin which rewrites type family applications, emitting wanted constraints and
throwing custom type errors as it goes, is provided [here](examples/RewriterPlugin/plugin/RewriterPlugin.hs).

A more complex example, that of type-checking an intrinsically typed System F, is provided
[here](examples/SystemF/src/SystemF/Plugin.hs). This implements the equational theory of the confluent calculus
of explicit substitutions from the paper [_Confluence properties of weak and strong calculi of explicit substitutions_](https://hal.inria.fr/inria-00077189):

<p align="center">
  <img src="img/substitution_calculus.png" alt="Rewrite rules for the confluent calculus of substitutions" />
</p>
