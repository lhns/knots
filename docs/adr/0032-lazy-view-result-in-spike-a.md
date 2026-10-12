# 0032. Spike A's build returns a lazy View

## Status

Accepted (2026-10-12, commit 70fe09f); whole-layer evaluation superseded by 0035

## Context

Spike A's `build` computed every field and returned the plain named tuple, which contradicts the lazy core decision (0015), and its caches were plain maps, not safe across threads.

## Decision

`build` returns a `View[F]` whose fields are computed on access; `toTuple` computes all of them. Base fields and override layers are cached in D's `Cell` (ScopedValue chain and wait-for graph), with cell names that keep A's layer labels.

## Consequences

A is lazy and thread-safe like the other spikes and needs JDK 25. Override layers are still evaluated as a whole (A has no macro to split them).
