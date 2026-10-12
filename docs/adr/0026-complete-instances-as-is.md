# 0026. Use complete instances as is

## Status

Accepted (2026-10-12)

## Context

A layer input can already be a complete instance of the target (a variable, `new Target { ... }`).

## Decision

Such an instance is used unchanged: no generated class, no caching, no cycle detection inside it, and overrides on top can't reach into its own fields.

## Consequences

Existing objects can be plugged in. Inside them, values aren't cached and a cycle can overflow the stack; this is documented.
