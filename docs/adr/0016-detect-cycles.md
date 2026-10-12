# 0016. Detect dependency cycles instead of hanging

## Status

Accepted (2026-10-12, commit 4b4f10c)

## Context

With Scala 3 thread-safe lazy vals, a cycle makes a lazy val wait on itself forever: the application hangs without a message. Relying on StackOverflowError was considered and rejected: with lazy vals a cycle deadlocks instead of overflowing, and catching and interpreting stack overflows is fragile.

## Decision

Every cached field goes through a cache cell that detects re-entry and throws `CycleException("cyclic dependency: a -> b -> a")` with the path.

## Consequences

Cycles fail fast with a readable message. The detection mechanism is decided in 0017 and 0019.
