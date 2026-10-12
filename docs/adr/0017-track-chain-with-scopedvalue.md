# 0017. Track the computation chain with a ScopedValue

## Status

Accepted (2026-10-12, commits 676fe7c, d9ac5e1)

## Context

Cycle detection needs to know which fields the current computation is in the middle of. A global ThreadLocal stack (4b4f10c) was rejected as shared mutable state tied to threads. Spike E passed the path explicitly through path-tagged `self`/`upper` copies, which needs no JDK feature, but it loses the path through closures: a cycle through a dependency passed into a module hangs, path-tagged copies have no stable identity, and the path leaks into threads started inside a computation.

## Decision

Use `java.lang.ScopedValue`: the chain is bound for the extent of each computation and visible to everything it calls, including closures created earlier. Spike E is kept for comparison.

## Consequences

Exact detection through modules and closures, no thread identity. Plain executors don't inherit the binding; StructuredTaskScope forks do. Requires JDK 25+ (0018).
