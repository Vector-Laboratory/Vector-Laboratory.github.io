# 8. Module System

## 8.1 Structure and Visibility

Each module is a single file. A module file contains exactly one top-level
module declaration with two optional sub-blocks — `interface` and
`implementation`:

```
module Name
  interface
    // public declarations (visible to importers)
  implementation
    // private definitions (internal to the module)
```

**Visibility rules:**

- Everything in `interface ( )` is exported and visible to any module that
  opens or qualifies this one.
- Everything in `implementation ( )` is private and inaccessible outside the
  module.
- The implementation block has full access to its own interface declarations
  plus all private definitions.
- If `interface ( )` is absent, the module exports nothing. It can still be
  used internally or as a namespace.
- If `implementation ( )` is absent, the module exports its interface only
  (useful for re-export aggregation modules).

A name declared in the interface (via a type signature `name : Type`) must be
defined in the implementation block. The compiler verifies completeness at the
module boundary.

## 8.2 What Belongs in Each Block

**Interface block** — declarations only:

- Type signatures: `name : Type`
- Data type declarations: `data Name ...` (with or without constructors; see §8.4)
- Distinct type declarations: `distinct Name ← Type`
- Type synonyms: `type Name ← Type`
- Effect declarations: `effect ...`
- Trait declarations: `trait ...`
- `derived` declarations: name and type signature only, exposing a read-only
  projection to importers (see 06_REACTIVE.md §6.5)
- Re-exported imports: `open OtherModule`

**Implementation block** — definitions and private declarations:

- Value bindings: `name ← expr`
- Hook definitions: `op`, `uop`, `iter`, `gen`, `app`, `project`, `assign`, `pat`, `from`
- `derived` definitions: full expressions with incremental maintenance (see 06_REACTIVE.md §6.2)
- Implementation blocks: `implementation TraitName ConcreteType ( ... )`
- Private helpers not in the interface (no type signature in interface)
- Private nested modules

**`derived` is not permitted in `trait` bodies.** A trait specifies abstract
operations, not incremental maintenance strategies; `derived` is an
implementation detail and cannot be part of a trait contract.

## 8.3 Imports and Re-exports

`open Module` brings all public names from `Module` into the current scope:

- **In the interface block**: `Module`'s public names become part of *this*
  module's interface. Importers of this module see those names without further
  qualification. This is re-export.
- **In the implementation block**: `Module`'s public names are available
  internally. Not visible to importers.

Qualified access `Module.name` is always available for any public name of
`Module`, regardless of `open` statements. Qualified paths may be chained for
nested modules: `Outer.Inner.name`.

**Circular imports are forbidden.** The module dependency graph must be a DAG.
The compiler rejects any import that would create a cycle. This preserves the
incremental invalidation model (01_EXECUTION.md §1.4) and the hook finalization guarantee
(§8.5).

## 8.4 Abstract Types

A data type is **transparent** if its constructors are exported, and
**abstract (opaque)** if only the type name is exported.

**Transparent** — full definition in the interface; constructors are exported:

```
// interface block:
data [a] Option ← None | Some a
// importers can pattern match on None and Some
```

**Abstract** — bare declaration in the interface; definition in the
implementation; constructors are private:

```
// interface block:
data Map k v                       // type name only; no constructors (no ←)

// implementation block:
data Map k v ← Leaf | Node (Map k v) k v (Map k v)
// Leaf and Node are private; importers cannot pattern match
```

Abstract types are the primary data-hiding mechanism. Public API is exposed
via type signatures in the interface block (smart constructors, accessors,
hooks). The `distinct` mechanism (02_TYPES.md §2.4) works identically:
declare the distinct type in the interface, define it in the implementation if
the underlying type should be hidden.

## 8.5 Modules as Coherence Boundaries

Modules are the unit of coherence for the orphan rule (03_HOOKS.md §3.4). The compiler
tracks, for each hook and trait implementation, which module defines it and
whether that module owns the relevant operator/trait and/or types involved.

**Hook resolution is per call site, using the importing module's scope.**
Each call site resolves its hooks using only the hooks visible at the
*containing module* — its own definitions plus its direct imports. A downstream
module B importing module A cannot retroactively affect A's internal call sites,
even if B defines a more-specific hook. A module's behaviour is determined
entirely by what it imports; no action-at-a-distance is possible.

Formally: hook sets are finalized at the **import boundary**. Once module A
is imported by B, A's internal call sites are fixed. B may define additional
hooks (subject to the orphan rule), but those hooks are only visible at
B's own call sites — they do not reach back into A.

The whole-program compiler uses this guarantee to resolve all hooks
statically at every call site — no runtime dispatch tables are generated.
Specialization (01_EXECUTION.md §1.3) instantiates type variables within the hook resolution
already established by the containing module's scope; it does not change which
hook fires, only what concrete types it is applied to.

**Interaction with `Program T` (07_METAPROG.md §7.3)**: quoted expressions use the hooks
visible at the quote site, consistent with the per-module rule. Splicing a
`Program T` into a downstream module does not re-resolve its hooks against
the downstream scope.

**Why the orphan rule makes this non-restrictive in practice**: for a downstream
module B to define a more-specific hook for a call site inside A, B would
need to own the operator *or* one of the concrete types involved. If B owns the
type, A's code cannot produce values of that type — there is no path to the
problematic call. If B owns the operator, A's use of that operator is constrained
by a trait bound (e.g., `Num`), and B's more-specific hook is reached
correctly through the trait's dispatch mechanism at B's own call sites. No
optimization opportunity is lost.

The compilation model (01_EXECUTION.md §1.4) means module boundaries are not compilation
barriers. The compiler sees all modules simultaneously; modules are
organizational and coherence units, not optimization fences.

## 8.6 Nested Modules

Modules may be defined inside `implementation` blocks. A nested module follows
the same structure as a top-level module. It is private by default. To
re-export it, include `open NestedModule` in the outer module's interface block.

```
module Outer
  interface
    foo : Int → Int
    open Inner            // re-exports Inner's public names
  implementation
    module Inner
      interface
        bar : Int → Int
      implementation
        bar ← x → x * 2
    foo ← x → Inner.bar x + 1
```

## 8.7 Open Questions

- **Functors (parameterised modules)**: ML-style module parameterisation
  (`module F(X : Sig) = ...`) is not yet specified. The trait system covers
  the majority of use cases. Deferred pending concrete need from implementation
  experience.

- **Module aliases**: a shorthand `module M = Other.Sub` for renaming or
  aliasing a nested module. Not yet specified.
