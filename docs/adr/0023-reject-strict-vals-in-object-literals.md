# 0023. Reject strict vals in object-literal inputs

## Status

Accepted (2026-10-12, commit 5bba376)

## Context

An object literal is created when the layer is applied, before `self` exists; a strict `val` that uses `self` failed at runtime, and in spike D all strict vals were evaluated together with the literal, so a cycle through them hung inside a lazy val.

## Decision

Public strict `val` members of object literals are compile errors ("use def (or lazy val), the layer caches it"); the layer caches each member anyway.

## Consequences

`def` is enough in object literals. Creating the literal evaluates nothing.
