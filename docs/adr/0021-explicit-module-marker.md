# 0021. Mark modules explicitly with module(...)

## Status

Accepted (2026-10-12, commit d5d3150, spikes D and E)

## Context

Module detection was type-directed and invisible: the same `db = x` meant a value or a module depending on `x`'s type. Alternatives: keep it implicit; an extension `layer.at(_.db)` without macros, which repeats the field name and can silently target another field of the same type.

## Decision

A module must be wrapped: `db = module(DbModule.base(...))`. `module` returns `BaseModule[B]` for an `Overridable` and `Module[B]` for an `Override` (`BaseModule <: Module`); a base accepts only `BaseModule`. It takes `using Self[?]` only so it can be used inside layers; the macro still inserts the focusing. A raw layer given for a module field is a compile error.

## Consequences

Module wiring is visible at the call site, with no field-name repetition and no wrong-field risk. Untied knots can still be stored as values in fields typed as layers or factories.
