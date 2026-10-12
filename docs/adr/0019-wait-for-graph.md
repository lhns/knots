# 0019. Detect cycles entered by several threads with a wait-for graph

## Status

Accepted (2026-10-12; implementation in progress)

## Context

Adversarial testing showed that a real cycle entered from two threads at once (thread 1 computes `a` and needs `b`, thread 2 computes `b` and needs `a`) deadlocks: each waits for the other's cell. Requirement: concurrent use must never deadlock on a real cycle, while staying lazy (0015).

## Decision

Before a computation waits on a cell that is being computed, it records "my chain waits on X" and follows X's waits; if they lead back into its own chain, it throws `CycleException`. Recording then checking under one lock makes simultaneous entry safe. Cells keep explicit states (empty, pending, done) for this.

## Consequences

Concurrent cycles fail instead of deadlocking. Not detectable: a computation blocking on something that isn't a cell (`executor.submit(..).get()`, `IO(..).unsafeRunSync()`) while the other side needs the computing cell; this deadlocks the same way with a plain lazy val and is documented.
