# 0004. Abandon the higher-kinded (tagless) case-class encoding

## Status

Accepted (2025-04-17, branch feature/base, commits b778999, eff9e95)

## Context

One attempt modelled dependencies as `case class MainDependencies[F[_]]` with cats-tagless `FunctorK`, so overrides could be `.copy(...)`. Tying the knot needs `F[Id]`, which is strict: building it forces fields that read `self`, so it failed with an NPE; making `self` lazy needed a recursive type alias, which Scala doesn't allow.

## Decision

Don't use the higher-kinded case-class encoding. Dependency sets are traits (or named tuples, 0013), and overriding is handled by the library.

## Consequences

No cats-tagless dependency, no per-type `FunctorK` instances, no `.value` noise in definitions.
