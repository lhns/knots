# 0035. Spike A uses a small macro for per-field overrides

## Status

Accepted (2026-10-13); supersedes the "macro-free" description of spike A in 0028 and the whole-layer evaluation noted in 0032

## Context

A partial override literal is typed as one context function `Upper ?=> O`, because which fields it contains is only known from the literal itself, so no expected type per field can be given (that works for the base, where F is known). Without macros, per-field deferral would need about 22 fixed-arity overloads or the field names repeated in the type. A macro can't change how the argument is typed, but it can split the typed literal: its elements are separate expressions.

## Decision

`Override[F]((...))` is backed by a small macro that finds the literal's elements (D's recognition helpers, with D's fixes) and emits one closure, and so one cell, per field. Non-literal inputs are evaluated as a whole once and read per field.

## Consequences

Reading one field of an override computes only that field, and fields of one override can use each other via `self`. A is no longer macro-free, but the macro is confined to `OverrideBuilder.apply`; `Partial`/`SubsetOf`, the base literal and modules stay macro-free.
