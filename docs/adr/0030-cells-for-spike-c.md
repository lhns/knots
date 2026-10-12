# 0030. Give spike C cycle detection through a generated wrapper

## Status

Accepted (2026-10-12, commit 44d9144)

## Context

In spike C the members are the user's own lazy vals, so a cycle hung (ADR 0011). That violates 0016 and 0019. Rewriting the user's anonymous class in place was considered and avoided as fragile tree surgery.

## Decision

The macro keeps the user's literal unchanged as a value and generates a wrapper class extending the target trait: each member the literal overrides goes through D's `Cell` (ScopedValue chain and wait-for graph) and reads the literal's member; the others keep the trait's `= inherited` default. Strict `override val`s are moved into the cell and become lazy.

## Consequences

C detects cycles like D, also across threads, and needs JDK 25. C now generates a class too, but the user's literal stays a real subclass of the trait.
