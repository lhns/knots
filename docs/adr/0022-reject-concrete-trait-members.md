# 0022. Reject concrete members of target traits

## Status

Accepted (2026-10-12; implementation in progress)

## Context

A member with a body in the target trait (`def b = a + 1`, `val`, `lazy val`, `var`) refers to other members through `this`, the layer object, not the final `self`, so overrides don't reach it. The macro can't rewrite trait bodies. Trait vals also initialise before the generated class's caches exist (NPE).

## Decision

Concrete members of a target trait (also inherited, and default-argument getters) are compile errors that say to make them abstract and define them in the base.

## Consequences

Target traits are pure interfaces. Defaults move into the base layer, where they are late-bound.
