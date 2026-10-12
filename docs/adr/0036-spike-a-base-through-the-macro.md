# 0036. Spike A's base goes through the same macro

## Status

Accepted (2026-10-13, commit 8dc1ee1)

## Context

After 0035 the base literal was still typed against an expected type derived from F (`Defs[F]`, a match type allowing modules). That failed above 22 fields (the expected type can't be spread over a `TupleXXL`) and for fields whose type is a named tuple (the match type doesn't reduce). The macro from 0035 already splits literals of any size.

## Decision

The base is split by the same macro as overrides (`Macros.split`, with `Self[F]` instead of `Upper[F]` as context) and checked field by field by the shared `checkFields`, which also requires every field of F. `Defs` and `BaseField` are removed. A non-literal base is evaluated as a whole, like a non-literal override.

## Consequences

Base and override share one path and one set of messages (`missing fields: ...`, "wrap it in module(...)"). Any number of fields and named-tuple field types work. Base fields no longer get the field type as expected type, so a lambda field needs explicit parameter types.
