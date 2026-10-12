# 0037. Spike A's module markers are Views; large bases are joined from parts

## Status

Accepted (2026-10-13)

## Context

The base literal's expected type used a match type (`BaseField`, 0033) to accept a module marker in a `View[Sub]` field. It doesn't reduce for a field whose type is a named tuple, so such bases didn't type. Separately, the compiler doesn't give a literal over 22 fields per-element expected types, so its fields aren't deferred as `Self[F] ?=> X`. Routing the base through the override macro fixed both but lost type inference for base fields (0036, rejected).

## Decision

`Module[S] extends View[S]`, so a marker fits its field's own type and `Defs[F]` is a plain `NamedTuple.Map[F, [X] =>> Self[F] ?=> X]`; the match types are gone. The `module` overload for an `Override` requires `Upper`, so it doesn't compile in a base. A base over 22 fields is written as parts, `Overridable.part[F, P]((...))`, joined with `++`. `part` returns its argument; its parameter type `Part[F, P]` (the fields P of F, deferred with `Self[F]`) gives each field its expected type, which `++` alone doesn't. `Defs[F] = Part[F, F]`.

## Consequences

The base stays macro-free, with per-field types and lambda inference, and named-tuple fields work. Large bases need the type declared in parts of at most 22 fields. A marker read as a view throws; a raw layer in a base module field is a plain type mismatch.
