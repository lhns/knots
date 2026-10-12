# 0015. Evaluate each field lazily and at most once

## Status

Accepted (2026-10-12; core decision)

## Context

Definitions refer to `self`, which only exists once the knot is tied, and many dependencies are expensive or must not be created in some setups (a test override replaces a real database connection). Eager initialisation at build time was considered because it would remove concurrency during initialisation.

## Decision

Every supplied field is computed on first access and cached (at most once, retried after an exception). Lazy evaluation is a core decision; eager build is rejected.

## Consequences

Overridden dependencies are never created. Concurrent first access and cycles must be handled at access time (0016, 0019).
