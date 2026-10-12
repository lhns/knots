# 0028. Explore designs as parallel spikes on one branch

## Status

Accepted (2026-10-12, branch spike/api-redesign)

## Context

Several designs had real trade-offs (named tuples vs traits, generated vs inherited delegates, cycle detection mechanisms) that were easier to judge by comparing working code.

## Decision

Keep spikes A (named tuples, no macros), B (generated delegates), C (trait as its own delegate), D (B plus named tuples, ScopedValue, modules) and E (D with explicit path passing) side by side in `src/main/scala/spike/*`, each with a demo, on branch spike/api-redesign. Lessons are applied across spikes; once one is chosen, the others are deleted (they stay in git history).

## Consequences

Duplication between spikes is intended until the choice. D is the current front-runner; E is kept for comparison and receives the shared fixes.
