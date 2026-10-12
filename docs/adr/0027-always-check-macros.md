# 0027. Always compile with -Xcheck-macros

## Status

Accepted (2026-10-12, commit 4ab1737)

## Context

The macros build trees by hand (moving user code, generating classes). Malformed trees can compile and still fail later or at runtime; an ownership bug was found only with tree checking.

## Decision

build.sbt sets `-Xcheck-macros`; all scratch verification runs use it.

## Consequences

Macro bugs surface at expansion time with a precise message. Slightly slower compilation.
