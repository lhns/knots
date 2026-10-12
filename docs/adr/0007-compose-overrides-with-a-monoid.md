# 0007. Compose overrides with a cats Monoid

## Status

Accepted (2026-10-12, commit 213528a)

## Context

Overrides were composed by varargs `apply`/`layer` functions. Stacking overrides is associative and has an identity (pass `upper` through).

## Decision

Provide `Monoid[Override[A]]` from cats-core (`o1 |+| o2`, `combineAll`). This adds the cats-core dependency.

## Consequences

Standard syntax and laws instead of custom combinators. The instance is named (`Override.monoid`) because inside the defining file the opaque type is transparent and cats' function instance would be found instead.
