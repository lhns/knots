# 0008. Overridable is a subtype of Override; Upper extends Self

## Status

Accepted (2026-10-12, commit 56ff8dc)

## Context

A base needs less than an override (only `self`), so it can stand wherever an override can. Merging both into one type, or reversing it (`Self` extends `Upper` with `upper = self` in a base), was considered to unify the API.

## Decision

Keep two types with `Overridable[A] <: Override[A]` (follows from `Upper[A] <: Self[A]` and function contravariance). `build` takes only an `Overridable`, the closed-at-the-bottom layer; `base |+| o` keeps a base a base.

## Consequences

`build(o1 |+| o2)` and `upper` in a base are compile errors. Merging or reversing would have turned both into runtime cycle errors.
