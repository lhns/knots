# 0006. Make Overridable and Override opaque types over plain functions

## Status

Accepted (2026-10-12, commit 5e39091)

## Context

As plain aliases of context-function types, a layer value is applied immediately wherever the expected type isn't exactly the alias: `val o = Override(...)` fails ("no given Super"), and `o1 |+| o2`, collections and extension methods don't work. Context-function types can't be opaque.

## Decision

`opaque type Override[A] = Upper[A] => A` and `opaque type Overridable[A] <: Override[A] = Self[A] => A`; `apply` accepts the context-function literal and wraps it.

## Consequences

Layers are ordinary values. Code in the defining file sees through the opaque types, so helpers that need that must live there or in the companions; the demos live in separate files.
