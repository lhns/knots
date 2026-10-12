# 0033. Spike A focuses modules at runtime

## Status

Accepted (2026-10-12, commit 4021ff7); the match types are superseded by 0037

## Context

Spike A has no macros, so nothing can insert the focusing for a module field at compile time (0020, 0021).

## Decision

A module field has type `View[Sub]` and takes `module(...)` (ordinary marker classes `BaseModule <: Module`). Match types in the base literal's expected type and in the override check allow a marker exactly where a `View[Sub]` field is. When A's runtime computes such a field and gets a marker, it ties the sub-knot focused through that field: `self` is the root's final view of the field, `upper` the layer below's. Cells in a module carry the module path in their names.

## Consequences

Modules work without macros, with late binding through the tree and cycle detection across modules. A raw layer in a base gets only a plain type-mismatch message.
