# 0025. Support parameterized dependency traits

## Status

Accepted (2026-10-12, commit 4aed517)

## Context

Effectful applications declare dependencies like `trait MainDependencies[F[_]] { def repo: F[Repo] }` and define them generically in `F`.

## Decision

Targets may be parameterized, with concrete or abstract type arguments (inside generic methods). Spike C's `inherited` uses the trait's applied type.

## Consequences

Tested with `Deps[Option]` and with `def base[F[_]]: Overridable[Deps[F]]` in spikes B to E.
