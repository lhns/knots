# 0001. Tie the knot for overridable dependency sets

## Status

Accepted (2025-04-17, commits 07b8ca7, e6c87c1)

## Context

An application's dependencies refer to each other (a repository needs a transactor, routes need a repository), and tests or environments need to replace single dependencies so that everything depending on them sees the replacement. Constructor wiring fixes those references when objects are created, so replacing one dependency means rewiring all its dependents by hand.

## Decision

Define a dependency set as a function of its own final result ("tying the knot", like `fix` in Haskell or Nix overlays): definitions refer to the finished set through `self`, which is only tied at build time. Overrides are layers on top that can also reach the layer below (`upper`), like `this` and `super` in class inheritance.

## Consequences

Late binding: an override changes every dependent, in any layer. The price is laziness (a definition can't read `self` while the knot is being tied) and cycle handling, see 0015 and 0016.
