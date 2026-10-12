# 0024. Reject overloaded members and abstract vals in target traits

## Status

Accepted (2026-10-12, commits 75519bf, 5bba376)

## Context

Fields are matched by name, so overloaded members broke the generated class silently; an abstract `val` can't be implemented by a generated `def` and produced a confusing compiler error.

## Decision

Both are compile errors: "overloaded member x in T is not supported" and "abstract val x in T is not supported; declare it as def x".

## Consequences

Clear errors at the source instead of malformed generated code.
