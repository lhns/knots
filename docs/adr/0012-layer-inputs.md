# 0012. Accept literals, class instances and complete instances as layer inputs

## Status

Accepted (2026-10-12, commits 5e39091, c3e8309, 33bc2d2)

## Context

Users want to write layers concisely and reuse existing code.

## Decision

A layer accepts: a named-tuple literal `(dep2 = ...)` (each field its own lazy computation), an object literal `new { def dep2 = ... }`, an instance of a named class (its declared members), a named-tuple value, or a complete instance of the target (used as is, 0026). Inputs are checked against the target at compile time (unknown field, wrong type, missing base field).

## Consequences

One `apply` covers all common forms. Recognising the named-tuple literal depends on how the compiler desugars it: the macro follows the literal's result, rejects statements in front of it, and handles more than 22 fields (d5d3150).
