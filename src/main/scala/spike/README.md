# Spikes: overridable dependency sets

Five designs of the same idea, side by side so they can be compared as working code. Once one is
chosen, the others are deleted (they stay in git history). The reasons behind each choice are in
[docs/adr](../../../../docs/adr/README.md).

## The shared idea

A dependency set is defined in terms of its own final result, and overrides are layers on top, like
class inheritance (see ADR 0001):

- `self` is the final, fully overridden set (`this`); `upper` is the layer below (`super`).
- `Overridable[A]` is a complete layer (a concrete class): it needs only `self` and can be built.
- `Override[A]` is a layer on top (a subclass): it needs `self` and `upper`, so it can't be built
  on its own.
- `Overridable.build(base, overrides*)` ties the knot. Layers compose with `|+|`: a cats `Monoid`
  for overrides, and `base |+| override` stays a base.

```scala
val base = Overridable[Deps](...)   // every field, may use self
val test = Override[Deps](...)      // some fields, may use self and upper
val deps = Overridable.build(base, test)
```

Every spike has a `Demo` that prints the same scenario: a base, two overrides (one using `upper`),
and the result built flat, layered and composed. A late-binding check is part of it: a base field
that uses `self.dep2` must see the overridden `dep2`.

## The spikes

| | A | B | C | D | E |
|---|---|---|---|---|---|
| Target | named tuple | trait | trait with `using Upper` | trait or named tuple (`View`) | trait or named tuple (`View`) |
| Override input | partial named-tuple literal | object literal, class instance, complete instance | `new T { override ... }` | named-tuple literal, object literal, class instance, named-tuple value, complete instance | same as D |
| Who implements unsupplied members | runtime lookup by field name | macro-generated delegate class | trait defaults (`= inherited`) | macro-generated delegate class | macro-generated delegate class |
| Laziness | per base field, each override layer as a whole (`Cell`) | per field (`Cell`) | per overridden member (`Cell`) | per field (`Cell`) | per field (`Cell`) |
| Cycle detection | `ScopedValue` + wait-for graph | `ScopedValue` + wait-for graph | `ScopedValue` + wait-for graph | `ScopedValue` + wait-for graph | explicit path + wait-for graph, with gaps |
| Modules (nested sets) | yes, focused at runtime | yes | yes, `module(...)` rewritten by the macro | yes | yes |
| Macros | none, only type-level code | generates a class | checks, and a wrapper class for the cells | generates a class | generates a class |
| JDK | 25+ | 25+ | 25+ | 25+ | 21+ |

### A: named tuples, no macros (`a/`)

`type Deps = (dep1: Dependency, ...)`. The base is a full named-tuple literal whose fields become
deferred context functions. An override is a partial literal, `Override[Deps]((dep2 = ...))`,
checked by type-level code (`SubsetOf`, reusable on its own together with `Partial`). `self`,
`upper` and the result of `build` are `View`s, a `Selectable` over the tuple whose fields are
computed on access; `toTuple` gives the plain tuple. Base fields and override layers are cached in
D's `Cell` (ADR 0032). `Overridable <: Override` as in the other spikes (a base can replace
everything below it); the override check (`Checks.scala`) rejects raw layers for module fields and
reserved field names (`selectDynamic`, `toTuple`, ...), while `Partial`/`SubsetOf` stay reusable on their own.

- Pros: no generated code at all; the checks are ordinary type-level Scala.
- Cons:
  - An override layer is evaluated as a whole, so fields of one override can't use each other via
    `self` (a cycle error says so).
  - `self.x` and the built object rely on IDE support for `Selectable`.
  - `Override[Deps]` always needs the type argument.
- Modules: a field of type `View[DbDeps]` takes `module(DbModule.base(...))`. A has no macro, but
  its runtime knows which field it is computing and ties the sub-knot focused through it (ADR 0033);
  cycle messages show the module path (`db.userRepo (base)`). See `ModulesDemo`.

### B: generated delegate for traits (`b/`)

`trait Deps { def dep1: Dependency; ... }`. A macro generates one class per layer that extends the
trait: members you supply forward to your input, the rest to `upper`. Inputs are object literals
(`new { def dep2 = ... }`), instances of named classes, or complete instances. Each supplied member
is cached in a `Cell` with `ScopedValue`-based cycle detection.

Modules work like in D, given as object-literal or class members:
`new { def db = module(DbModule.base(self.config.dbUrl)) }` (see `ModulesDemo`).

- Pros:
  - Nominal types everywhere, so the IDE behaves as usual.
  - No delegate boilerplate.
  - A bare `dep1` without `self.` doesn't compile.
  - Modules and D's cycle handling, with a smaller macro than D.
- Cons:
  - No named-tuple literals (that is D).
  - Needs JDK 25.

### C: the trait is its own delegate (`c/`)

`trait Deps(using Upper[Deps]) { def dep1: Dependency = inherited; ... }`, where `inherited` is a
small macro expanding to `upper.dep1`. Overrides are real subclasses:
`Override[Deps](new Deps { override lazy val dep2 = ... })`. The macro checks that the base overrides
everything and doesn't use `upper`, that no member is used bare, and that every concrete trait
member is exactly `= inherited`. It wraps your literal in a generated class whose overridden members
go through D's `Cell`, so cycles are detected like in D (ADR 0030); a strict `override val` becomes
lazy that way.

- Pros:
  - The most native IDE experience ("override member" completion, typos show as you type).
  - Your literal stays a real subclass; the wrapper only adds caching and cycle detection.
- Cons:
  - The domain trait carries plumbing: the `using Upper[...]` parameter and one `= inherited` line
    per member. Mocks need an `Upper` too.
  - Checking that trait members are `= inherited` needs `-Yretain-trees` (ADR 0031).
  - Only object literals as input; no named tuples.
  - Modules work as `override lazy val db = module(DbModule.base(...))`: `module(...)` is typed as
    the member and the macro rewrites it (ADR 0034). A `module(...)` that escapes the macro (nested
    inside another expression) only fails at runtime.
  - Needs JDK 25.

### D: B plus named tuples, modules and full cycle handling (`d/`, front-runner)

Everything from B, plus:

- Named-tuple literals as input, each field its own lazy computation; `View[F]` targets for named
  tuples (stage 2 of unifying A and B).
- Modules: a field like `db: DbDeps` is given another layer with `db = module(DbModule.base(...))`
  and runs focused through the field. Late binding then works across the whole tree, and a nested
  override is a deep override.
- `ScopedValue` follows the actual call chain, including closures, so cycles across modules are
  detected.

- Pros: covers every requirement found so far: lazy, late-bound, modules, parameterized traits,
  cycles, concurrency, nominal trait targets.
- Cons:
  - The largest macro.
  - Needs JDK 25.
  - `View` targets have the same `Selectable` IDE question as A.
  - A cycle through work handed to another thread inside a computation
    (`executor.submit(...).get()`) can't be detected, like with a plain `lazy val`.

### E: D with an explicit path instead of `ScopedValue` (`e/`)

Same API as D, but the cycle path is passed explicitly: user code gets path-tagged copies of `self`
and `upper`, and each field computation gets its own copy. Object-literal members may only use
`self`/`upper`, not each other.

- Pros: runs on JDK 21; no dynamic scoping.
- Cons, inherent to the explicit path:
  - A `self` captured in a closure keeps an old path. A cycle through a dependency passed into a
    module therefore hangs, and a thread started inside a computation can get a false cycle error.
  - `self` copies have no stable identity (`self eq self` is false).
  - Complete instances lose the path.
  - More generated code than D.
- Kept for comparison; it receives the same fixes as D.

## What they have in common

- `Self`/`Upper`, the `self`/`upper` accessors, and the inheritance names (ADRs 0002, 0003).
- Opaque layer types in B to E: `Overridable[A] <: Override[A]`, both plain functions (ADRs 0006,
  0008). A uses classes.
- `build(base, overrides*)`, `|+|` and the cats `Monoid` (ADRs 0007, 0009).
- Compile-time checks in every spike: unknown field, wrong type, missing base field, and a bare
  field name without `self.` (structurally in A, B, D and E, by a macro in C).
- Parameterized traits like `Deps[F[_]]` work in B to E (ADR 0025).
- Built with `-Xcheck-macros`, so malformed generated code fails at compile time (ADR 0027).

## Current state

- Applied to D and E after adversarial testing:
  - the required `module(...)` marker (ADR 0021);
  - rejecting concrete members of target traits (ADR 0022);
  - the wait-for graph, so a cycle entered by several threads throws in each of them (ADR 0019);
  - robust recognition of named-tuple literals (statements before it are an error, more than 22
    fields stay lazy, `++` works);
  - one clear error for wrong inputs, and guards for unusual traits.
- Brought to A, B and C as far as they apply: D's `Cell` with the wait-for graph everywhere, lazy
  `build` in A, modules in B, cycle detection in C, and the input checks, guards and messages in B
  and C.
- Remaining gaps in D: a cycle through work handed to another thread inside a computation
  (`executor.submit(...).get()`), complete instances (not cached, no cycle detection), a named
  class reading `self` in its constructor, very deep `upper` chains.
- The open question for the final choice is how IntelliJ handles `self.x` on a `View`. For trait
  targets in D the IDE question doesn't arise.
