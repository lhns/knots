# 0031. Compile with -Yretain-trees

## Status

Accepted (2026-10-12, commit 44d9144)

## Context

Spike C requires every concrete member of a target trait to be exactly `= inherited` (0022 adapted to C). Checking that needs the member's body, which `Symbol.tree` only provides with `-Yretain-trees`. Reading the member's source text instead was tried and rejected as fragile.

## Decision

build.sbt sets `-Yretain-trees`. Without it the check is skipped (the member can't be inspected).

## Consequences

Trait bodies are checked in the current compilation and in libraries compiled to TASTy. Slightly more memory during compilation.
