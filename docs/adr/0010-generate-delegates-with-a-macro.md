# 0010. Generate the delegating class with a macro

## Status

Accepted (2026-10-12, commits bb61de8, 4ab1737, c3e8309)

## Context

Overriding a member of a trait-shaped dependency set needed a hand-written `Delegate` class forwarding every member to `upper`. `export` can't replace it (export forwarders are final). A first version implemented the trait at runtime with `java.lang.reflect.Proxy`.

## Decision

A macro generates, per layer, a class extending the target trait: supplied members forward to the user's input, the rest to `upper`. Uses only stable quotes-reflect API (`Symbol.newClass`, `ClassDef.apply` are not experimental in 3.9). No reflection at runtime.

## Consequences

No delegate boilerplate and no Proxy. The generated class is invisible to the user (only the trait type is exposed), so IDE support is unaffected. Macro code must be kept robust (see 0027).
