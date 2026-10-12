# 0003. Name the parts after class inheritance

## Status

Accepted (2025-04-17 to 2026-10-12, commits 4582909, 627f972, ea24163, 3c4b8f9)

## Context

The first names (`Knot`, `KnotEnd`, `DoubleKnot`, `tieKnotWith`) described the mechanism, not its use. Internal names later mixed metaphors (`stack(lower, ovr)` with `upper = lower(s)`), which was confusing.

## Decision

Follow class inheritance throughout: `Overridable` (a complete, buildable layer, like a concrete class), `Override` (a layer on top, like a subclass), `Self`/`self` (the final `this`), `Upper`/`upper` (`super`; renamed from `Super`), `layer(parent, child)` for stacking.

## Consequences

One analogy explains the whole API. `Upper` also avoids a name clash with `quotes.reflect.Super` in macros.
