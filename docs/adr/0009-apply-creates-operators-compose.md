# 0009. apply creates layers; |+| and build compose

## Status

Accepted (2026-10-12, commits 5e39091, 56ff8dc)

## Context

`apply` overloads that both created and composed layers needed a "two or more arguments" workaround, because overload resolution can't tell a context-function literal from a value.

## Decision

`Overridable(...)`/`Override(...)` only create. Composition is `|+|` (Monoid, plus an extension keeping a base a base); `build(base, overrides*)` reuses it.

## Consequences

No overload tricks; one way to compose.
