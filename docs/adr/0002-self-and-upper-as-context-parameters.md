# 0002. Pass self and upper as context parameters

## Status

Accepted (2025-04-17, commits 07b8ca7, e6c87c1)

## Context

Definitions need access to the final set and to the layer below without threading parameters through every definition.

## Decision

A layer is code with a given `Self[A]` (base) or `Upper[A]` (override) in scope; `self` and `upper` are accessors that summon it. Layers are written as `Self[A] ?=> A` / `Upper[A] ?=> A`.

## Consequences

Definitions read like ordinary code (`self.dep2`, `upper.dep2`). A context function type as a value type has drawbacks, see 0006.
