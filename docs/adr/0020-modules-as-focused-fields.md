# 0020. Compose modules as fields, focused through the field

## Status

Accepted (2026-10-12, commits cd1b466, d9ac5e1)

## Context

Larger applications consist of modules (database, server, ...) that are defined and tested separately, depend on each other, and must be overridable deep inside from the top. A flat "cake" (`MainDeps extends DbDeps with ServerDeps`) loses namespacing and causes name clashes.

## Decision

A field whose type is a dependency trait (`db: DbDeps`) can be given that trait's layer; it runs focused through the field: its `self` is the root's final `db`, its `upper` the layer below's `db`. Modules take what they need from outside as (by-name) parameters, not as abstract members, so a module's base stays complete.

## Consequences

Late binding works across the whole tree, deep overrides are nested overrides, and two apps built from one module share nothing. Building a module separately (`Overridable.build`) inside a field is allowed but closes that knot.
