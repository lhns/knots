# 0034. Spike C types module(...) as the member and rewrites it

## Status

Accepted (2026-10-12, commit 4021ff7)

## Context

In spike C an override member must have the member's type (`override lazy val db: DbDeps`), so a marker type like D's `Module[DbDeps]` doesn't fit.

## Decision

`module(...)` returns the member type. C's macro rewrites a member whose whole body is `module(layer)` into the focused module (like D's `focus`). Any other `module(...)` call in the literal is a compile error; one that still escapes the macro (nested inside a module's own argument) throws at runtime.

## Consequences

C gets modules with its normal override syntax. Slightly more magic than D: the call looks like an ordinary value but is replaced.
