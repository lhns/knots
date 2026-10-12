# 0005. Use Scala 3.9 for named tuples

## Status

Accepted (2026-10-12, commit ef7596e)

## Context

Named tuples and `Selectable` with a `Fields` type are experimental in Scala 3.6 and standard since 3.7. Both are used for named-tuple literals as input and for named-tuple targets.

## Decision

Build with Scala 3.9.0 (instead of 3.6.4).

## Consequences

Requires JDK 17+ (Scala 3.8+), in practice JDK 25+ because of 0018.
