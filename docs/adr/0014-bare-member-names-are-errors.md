# 0014. Bare member names are compile errors

## Status

Accepted (2026-10-12)

## Context

In a trait-based layer, writing `dep2` instead of `self.dep2` silently binds to the current layer and skips overrides on top of it.

## Decision

Layer inputs don't extend the target trait (named-tuple and object literals), so a bare member name doesn't resolve and is a compile error ("Not found: dep2"). Spike C needs a macro check for the same rule.

## Consequences

The most common late-binding mistake can't compile. Concrete members inside the target trait have the same problem and are rejected (0022).
