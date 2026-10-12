# 0029. Make the partial named-tuple check reusable

## Status

Accepted (2026-10-12, commit bf92ba8, spike A)

## Context

Spike A checks overrides given as partial named tuples with type-level logic. The user wanted that logic reusable outside the library.

## Decision

Provide `O SubsetOf F` (evidence that every field of O exists in F with a conforming type, errors naming the field) and `Partial[F] = NamedTuple.Map[F, Option]` with `Partial[F](o)` filling missing fields with None, independent of `Overridable`.

## Consequences

Usable on its own. A partial literal can never be a subtype of the full tuple type, so `Partial[F](...)` must be called explicitly.
