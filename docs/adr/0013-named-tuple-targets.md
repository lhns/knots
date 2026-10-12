# 0013. Support named-tuple targets through View

## Status

Accepted (2026-10-12, commits ef7596e, 33bc2d2)

## Context

Some users prefer declaring a dependency set as a named tuple instead of a trait. A tuple can't exist before its fields are computed, so `self`/`upper` can't be plain tuples.

## Decision

A named tuple F is a target as `View[F]`, a `Selectable` with `Fields = F` whose generated implementation dispatches to lazily computed members; `toTuple` produces the plain tuple.

## Consequences

`self.dep2` on a View relies on IDE support for `Selectable` fields, which is unverified in IntelliJ. Trait targets keep full nominal types.
