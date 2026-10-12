# 0011. Prefer generated delegates over a trait that is its own delegate

## Status

Accepted (2026-10-12, commit 2365192, spike C)

## Context

Alternative: the trait takes `using Upper[...]` and every member defaults to `inherited` (forwarding to `upper`); overrides are `new Trait { override ... }`. It needs no class generation and gives native IDE override completion.

## Decision

Keep spike C as a comparison, but prefer generated delegates (spikes B/D).

## Consequences

Spike C puts plumbing into the domain trait, needs a macro to catch bare member names, and can't detect cycles because members are the user's own lazy vals.
