# Arra — Backend

The backend receives Core IR from the frontend and produces either interpreted
output (treewalk) or compiled C (compiler). Core IR is the split point:
everything above it is implemented once in OCaml; everything below diverges
into two independent backends. Both backends must produce identical outputs for
all well-typed programs — any divergence is a backend bug.

---

## 1. Treewalk Interpreter

Interprets Core IR directly using OCaml 5's native effect system. No
optimization, no fusion, no kernel selection. Readable over fast; the
definition of correct. The QCheck oracle for all compiler passes.

**Effect mapping.** Arra's `ctl`/`op` distinction maps directly onto OCaml 5:

- `ctl` operations — non-resumable. Map to OCaml 5 effects whose handler does
  not invoke the continuation. No explicit continuation threading needed.
- `op` operations — resumable. Map to OCaml 5 effects whose handler calls the
  continuation one or more times.

`Handle`/`Perform` nodes in Core are interpreted via `match_with`. No CPS
encoding, no `ContT`, no stack manipulation — algebraic effects implemented in
algebraic effects.

**Other nodes:**
- `iter` / `gen` — structural recursion over the concrete collection type
- `derived` — full recompute on every access; no delta machinery, no DAG
- `from` / `iso` coercions — applied eagerly at every site
- Tabled predicates — per-query memo table with fixpoint iteration
- `prim.assert_dim` — live bounds assertions; failure is a runtime error
- `Extern` — treewalk error for call-form externs; expression-body externs are
  compile-only permanently. **Decision: compile-only initially; dynamic loading
  in a later phase.** Once the REPL is a primary development surface, call-form
  `Extern` nodes will be supported via `dlopen`/`dlsym` at startup and OCaml
  ctypes for dispatch (the type mapping table in [semantics/09_FFI.md §9.4](semantics/09_FFI.md#94-type-mapping) doubles
  as the ctypes descriptor spec). Expression-body externs (macro wrapping) cannot
  be evaluated at OCaml runtime without a C compiler; the treewalk error message
  at an expression-body site makes this distinction explicit. The QCheck harness
  never generates `Extern` nodes, so the treewalk's oracle role is fully
  preserved for all tested properties.

---

## 2. Whole-Program Passes

Run over Core IR before ANF/closure conversion and C emission.

### 2.1 Specialization / Monomorphization

Instantiates type variables with concrete types at each call site. Produces a
**specialization graph**: each node is a monomorphic function instance.
Whole-program pass — runs after all hook resolution is final. No typeclass
dictionary passing; hooks are fully resolved before Core.

**Value-level dispatch.** When a hook dispatches on a literal value (e.g.
`iter 'rank` on rank `1` vs `2`) and the value is a compile-time constant, the
hook is selected statically. When it is a runtime value, the compiler generates
monomorphized code for each reachable case and branches at runtime. A generic
runtime branch table is the fallback only when reachable cases cannot be
determined statically.

### 2.2 Effect-Based Specialization

At each call site, the concrete effect row is now known. This pass annotates
each operation:

- **Pure** (`e = ⟪⟫`): fusion-eligible, freely parallelizable. The compiler
  may reorder, vectorize, or eliminate intermediate arrays.
- **Effectful** (`e ≠ ⟪⟫`, `Distributable` satisfied): sequentially ordered
  left-to-right. No parallelization or fusion applied.

Runs after monomorphization; annotations consumed by the fusion passes.

### 2.3 Layer 1 — Algebraic Rewriting

The fusion passes in §2.3 and §2.4 operate by pattern-matching on **SOAC
primitive markers** (`prim.vec_map`, `prim.vec_filter`, `prim.vec_fold`, etc. —
see [CORE.md §8.2](CORE.md#82-soac-primitive-markers)). User loops written with raw `prim.vec_get`/`prim.vec_set`
and all `Extern` FFI calls are **fusion-opaque** and pass through unchanged.

Unconditional term rewrites at the IR level, before structural analysis. Always
correct regardless of rank or size.

```
arr 'map g 'map f              =  arr 'map (f ∘ g)
arr 'filter p 'filter q        =  arr 'filter (x → p x && q x)
arr 'map f 'filter p           =  arr 'mapMaybe (x → if p (f x) then Some (f x) else None)
arr 'mapMaybe g 'map f         =  arr 'mapMaybe (x → g x 'map f)
arr 'map f 'fold ⊕ z           =  arr 'fold (acc x → acc ⊕ (x f)) z
arr 'filter p 'fold ⊕ z        =  arr 'fold (acc x → if p x then acc ⊕ x else acc) z
arr 'map f 'scan ⊕             =  arr 'scan (acc x → acc ⊕ (x f))
arr 'flatMap g 'map f          =  arr 'flatMap (x → g x 'map f)
arr 'map f 'flatMap g          =  arr 'flatMap (x → x f g)
```

Array identities (distributivity of reduction over arithmetic, associativity
of append, etc.) are applied in this layer too.

### 2.4 Layer 2 — SOAC Fusion

Post-monomorphization and post-Layer-1. Classifies pure array operations by
streaming role and merges adjacent eligible operations.

**Eligibility**: effect row is empty (`e = ⟪⟫`) and operation is map-like or
reduce-like. Statically decidable after §2.2.

**SOAC taxonomy:**

| Category | Operations | Fusion role |
|---|---|---|
| Map-like | `'map`, `'filter`, `'mapMaybe`, `'flatMap`, `'scan`, `'zipWith` | Fuse upstream and downstream; no materialization |
| Reduce-like | `'fold`, `'sum`, `'product`, `'any`, `'all`, `'count` | Terminates a vertical chain; fuses with one upstream producer |
| Materialize-required | `'sort`, `'groupby`, `'asofJoin`, `'distinct`, `'rank` (runtime `r`) | Fusion boundary; upstream chain materializes first |

**Vertical fusion:** a chain of eligible operations compiles to a single loop.
No intermediate arrays allocated. Any sequence of map-like operations followed
by at most one reduce-like consumer.

**Sigma type does not force materialization.** `'filter` returns
`∃(m ≤ n) a[m]` — a type, not an allocation. Elements stream lazily; the
downstream consumer runs in the same loop. Materialization only occurs at a
materialize-required operation.

**Horizontal fusion:** multiple pure reduce-like operations over the same source
fuse into a single pass with a tuple accumulator:

```
// Before:
arr 'fold (+) 0
arr 'fold (*) 1

// After — single pass:
arr 'fold (acc x → (acc.1 + x, acc.2 * x)) (0, 1)
```

Generalizes to `n` folds. For mixed consumers (e.g. one fold and one sort),
the sort is a materialization boundary; the fold fuses independently.

**Deferred:** rank-polymorphic horizontal fusion and `'scan` horizontal fusion.
The `'scan` case requires careful dependency analysis (each step depends on the
previous); deferred to implementation experience.

### 2.5 Layer 3 — Generator-Consumer Fusion

`⟪Nondet⟫` generators fuse with short-circuiting consumers:

- `'first (pred arr)` — early-exit loop; no generator object allocated.
- `'any (pred arr)` — same; halts on first success.
- `'all (pred arr)` with a pure predicate — eligible for Layer 2.

A generator object is only allocated when the computation genuinely suspends
and resumes — which `'first` and `'any` never do.

### 2.6 Derived DAG Construction

For each `derived` binding, the compiler:

1. Analyses dependencies over `⟪State⟫` cells via static data-flow.
2. Constructs the dependency DAG — nodes are `derived` views, edges are source
   dependencies.
3. Determines incrementalization level per operation:
   - **Level 1** — built-in SOACs (`'map`, `'filter`, `'groupby`, equi-`'join`,
     `'count`, `'sum`, `'distinct`): O(1) per element change, automatic.
   - **Level 2** — `/'-CMonoid -'/` annotation: derive delta from `op +` / `op -`
     / `from` zero. Verified by property-based tests at compile site.
   - **Level 3** — `/'-Delta f -'/` annotation: explicit delta function, pure,
     handles non-algebraic structures (sorted indexes, graph reachability, etc.).
     Debug builds insert a consistency assertion against full recompute.
   - **Level 4** — no annotation: full recompute after each relevant change.
     A note (not warning) identifies the operation that forced fallback.
4. Compiles delta functions — required to be pure (no effect row); enforced by
   the type system.
5. Compiles full-recompute functions alongside delta functions for debug
   consistency checking.

**Update ordering:** topological over the dependency DAG. No `derived` value is
updated until all its `derived` dependencies have been updated for the current
event. The entire update pass is a compiler-internal transaction: no query or
external caller can observe any `derived` value until all updates complete.

**Atomicity:** delta functions are pure (no `⟪IO⟫`). Since IO is the only way
for external code to observe state, no intermediate derived state is ever
externally visible. No mutex or snapshot-swap required.

**Diamond dependencies:** when view `c` depends on views `a` and `b` which both
derive from the same source, `c`'s delta function is called once per upstream
change in DAG-topological order. Between calls, `c` is not externally
observable.

**Delta failure:** on failure mid-transaction, the event is marked failed in the
log, the partial update is discarded, and a Level 4 full recompute restores
consistency. Surfaces as `⟪Fail DeltaError⟫` to the event handler's caller.

### 2.7 Kernel Selection

After fusion, each fused operation sequence is matched against the primitive
kernel library by element type, size class, and operation pattern. This
information is determined exactly by the fusion passes — kernel selection is the
primary payoff of doing fusion before codegen. Sequences not covered by the
kernel library fall through to generic C emission.

---

## 3. Effect Lowering to C

Two-tier dispatch. `ctl` and `op` compile differently because their continuation
requirements differ.

### 3.1 `ctl` — Static Handler Resolution

The whole-program compiler traces each `ctl` call site to its nearest enclosing
`Handle` in the call graph.

**Statically-known handler** (common case — concrete `Handle` a fixed number of
call frames away): `ctl` compiles to a **direct early return**. No runtime
handler lookup, no evidence frame, no stack manipulation. For `⟪Fail E⟫`,
`ctl raise e` compiles identically to returning `Err e` — the effect is fully
erased after type-checking.

**Statically-unknown handler** (function polymorphic over an effect row,
handler supplied by caller): **evidence passing**. The function receives an
implicit handler-frame pointer for each `ctl` effect in its row. `ctl raise e`
dispatches through that frame. Whole-program specialization eliminates the
evidence parameter whenever the concrete caller is known.

### 3.2 `op` — Evidence Passing

Resumable operations always require a captured continuation, so evidence passing
is used uniformly regardless of whether the handler is statically known.

Each function polymorphic over a resumable effect carries an **implicit evidence
parameter** — a struct containing the handler clause as a closure.
`op yield x` looks up the effect's evidence frame, passes `x` to the handler
body, and passes `k` (the explicit continuation) as the final argument.

**Continuation `k`**: `k : A → ⟪rest⟫ R` — an ordinary function value. May be
called zero times (prune), once (resume), or more than once (fork, as in
`⟪Nondet⟫`). Heap-allocated when the computation suspends past the handler
body's dynamic extent; stack-resident when the handler calls `k` immediately
and does not store it.

Whole-program compilation specializes evidence parameters away at concrete call
sites, replacing frame lookups with direct calls.

### 3.3 Evidence Frame Layout

An evidence frame for effect `E` is a C struct containing:

- For each `ctl` operation: a function pointer `(args, frame*) → result`
- For each `op` operation: a function pointer `(args, k, frame*) → result`
- A link to the outer frame (for nested handlers and effect delegation)

**Non-escaping handlers** (the common case — handler struct lifetime bounded by
the `Handle` expression): frame is **stack-allocated** at the `Handle` site.
Zero heap allocation.

**Escaping handlers** (`k` is stored and called outside the handler body's
immediate dynamic extent): frame must be heap-allocated. The compiler detects
this via escape analysis on `k`.

---

## 4. ANF / Closure Conversion

**ANF conversion:** puts Core into administrative normal form — every
intermediate result is named, evaluation order fully explicit. Eliminates
duplicate evaluation and makes calling conventions explicit before C emission.

**Closure conversion:** converts lambda expressions to explicit closure records
(function pointer + captured-variable struct). Required before C emission;
C has no native closures. Evidence frames (§3) are a special case of closure
records.

---

## 5. C Emission

Emits C for the structural parts of the program.

- **Coercions** — `from`/`iso`/`distinct` coercions desugar to
  `prim.unsafe_coerce` calls. An early inline pass eliminates all such
  applications before C emission; no dedicated `Coerce` node in Core is needed.
- **Control flow** — conditionals, loops, early returns
- **Closures** — struct + function pointer pairs from closure conversion
- **Effect handlers** — CPS; `ctl` handlers as direct return paths, `op`
  handlers via evidence frame dispatch
- **Delta maintenance functions** — pure C, no side effects; signatures carry
  no effect annotation
- **Full-recompute functions** — emitted alongside delta functions; used in
  debug builds for consistency assertions against delta results
- **FFI call-form** — direct C function call at each use site
- **FFI expression-body** — fresh C block with re-bound argument locals
  followed by the injected expression string; see [semantics/09_FFI.md §9.5](semantics/09_FFI.md#95-expression-body-externs)

**GC pinning.** Only `Ptr` and pinned-arena-allocated types are valid at FFI
call sites. The emitter never pins arbitrary heap objects — GC-managed values
require an explicit `.pin` or `.copy` call at the boundary, visible in the
type.

**Callbacks.** Closures with an empty effect row lower to a plain C function
pointer. Effect-carrying or resumable closures are a hard type error at the
call site. The `/'-Callback-'/` attribute is reserved for a future trampoline
mechanism.

Hot array primitives are **not** emitted as C loops. Kernel selection (§2.7)
has matched fused sequences to hand-written kernels; the emitted C calls into
these kernels directly. Clang/GCC handles the C; the kernels handle the
performance.

---

## 6. Primitive Kernel Library

Hand-written, SIMD-optimized kernels covering the most frequent fused operation
patterns. Implemented in C with inline assembly (SSE2/AVX2 on x86, NEON on
ARM).

**Design principle:** each kernel is hand-tuned to the maximum achievable
throughput on the target hardware. The kernel library is the ground truth for
performance — the long-term goal of a Loop IR (§8.3) is to generate code that
matches these kernels automatically for any fused pattern.

**Initial coverage:**

| Pattern | Notes |
|---|---|
| `'map f` | Type-specialized: float32, float64, int32, int64, bool (bit-packed) |
| `'map f >> 'fold g` | Fused map-reduce; SIMD accumulator |
| `'filter p >> 'map f` | Predicated map; AVX-512 mask registers where available |
| `'map f >> 'map g` | Kernel composition; eliminates intermediate array |
| `'zip >> 'map f` | Paired element operation |
| `'fold g` | Reduction: sum, max, min, dot product; type-specialized |

**Runtime dispatch:** each kernel entry point dispatches on size class
(small / medium / large) and SIMD capability (SSE2 / AVX2 / AVX-512 / NEON).
Single branch computed once per call; does not affect throughput for large
arrays.

**QCheck coverage:** for every fused pattern covered by the kernel library, the
QCheck harness verifies kernel output matches treewalk output across element
types, array sizes, and all SIMD code paths. Kernels are the most
performance-sensitive code in the system and therefore the most important to
test.

---

## 7. QCheck Harness

Generates well-typed Arra programs and checks that treewalk and compiler agree
on all outputs.

**Generator:** produces well-typed Core IR programs directly, bypassing the
parser. Initial coverage: first-order functions, `⟪Fail⟫` and `⟪State⟫`
effects, rank-1 arrays with concrete dimensions, `derived` expressions over
simple `iter` chains.

**Property suites:**

- **Fusion correctness** — fused output matches unfused treewalk output for all
  generated `iter` chains
- **Kernel correctness** — for every fused pattern in the kernel library, kernel
  output matches treewalk output across element types, array sizes, and SIMD
  code paths
- **Derived correctness** — `derived` result after each delta matches full
  recompute from treewalk
- **Effect handler semantics** — compiled two-tier `ctl`/`op` dispatch matches
  treewalk OCaml 5 effect interpretation
- **Coercion round-trips** — for every `iso` declaration, both round-trip laws
  hold under both backends
- **Splice determinism** — Program T expansion produces the same typed AST
  regardless of evaluation order within a pass

**Shrinking:** standard QCheck shrinking on the Core IR AST. Failing cases
shrink to minimal programs.

---

## 8. Open Questions

### Loop IR

A structured loop representation between the fusion passes and C emission.
See [LOOP-IR.md](LOOP-IR.md) for the full design. This is a later phase and does not
affect Core's design. Tracked here as the primary long-term backend work item.
