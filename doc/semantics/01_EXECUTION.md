# 1. Core Execution Model

## 1.1 Multiple Dispatch

The central execution mechanism is hook dispatch. Rank polymorphism
and iteration hooks are understood as special cases of hook dispatch
on array types, where the dispatch also determines how iteration is applied to
the arguments. This reframing is the foundation of Arra's type system.

## 1.2 Word Order

Evaluation is left-to-right (SVO). Value binding uses `←`: the right-hand
side is evaluated, then bound to the left-hand pattern. Type annotation uses
`:` exclusively — it is not a binding operator. Type application is LTR;
constructor application (value construction) is RTL.

## 1.3 Value-Level Dispatch and Specialization

Hooks may dispatch on literal value patterns as well as types:

```
iter 'rank a[n;m], (1 : Int), (a[] → a) → a[n] ← prim.rank1
iter 'rank a[n;m], (2 : Int), (a[] → a) → a[m] ← prim.rank2
```

When the dispatching value is a compile-time constant, the appropriate hook
is selected statically. When it is a runtime value, **specialization** is used:
the whole-program compiler generates monomorphized code for each reachable case
and branches at runtime. A fully generic runtime branch table is the fallback
only when the set of reachable cases cannot be determined statically. See
Section 1.4 on the compilation model.

## 1.4 Compilation Model

Arra uses **whole-program compilation**. The module system is a source-level
organizational and coherence concept, not a compilation boundary. The compiler
sees all modules simultaneously, enabling:

- Specialization of value-level dispatch across any call site in the program
- Compile-time resolution of all hooks (no runtime dictionary passing)
- Full visibility of fusion and rewriting opportunities across module seams

**Specialization and hook resolution are independent.** Specialization
instantiates type variables with concrete types; it does not change which
hook is in scope at a call site. Hook resolution uses the importing
module's scope ([08_MODULES.md §8.5](08_MODULES.md#85-modules-as-coherence-boundaries)). Specialization runs after resolution, filling in
concrete types for the already-resolved hook.

**Incremental compilation**: modules are the unit of invalidation. When a
module changes, all modules that directly or transitively import it are
recompiled — the full transitive closure of reverse dependencies. Whole-program
optimization is re-applied over the invalidated set. Modules outside the
invalidated set are not touched; their cached Z3 results and generated code
remain valid.
