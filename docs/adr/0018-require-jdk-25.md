# 0018. Require JDK 25 or newer

## Status

Accepted (2026-10-12, commit 676fe7c)

## Context

`ScopedValue` (0017) is final since JDK 25 and a preview API in JDK 21.

## Decision

Build and run on JDK 25+; build.sbt fails early with "needs JDK 25+ for ScopedValue" on older JDKs.

## Consequences

Users on JDK 21 can't use the library. Spikes A to D need JDK 25; only spike E runs on 21, but it isn't the chosen design. IDE project SDKs must be 25+.
