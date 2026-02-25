# 5. Effect System

## 5.1 Row-Typed Effects

Effects are tracked in the type system as rows. A function with effects has a
type of the form `a → ⟪eff⟫ b`, where `⟪eff⟫` is a row of effects. Effect
polymorphism allows functions to be generic over effects.

Arra uses **full algebraic effects with user-definable handlers**. Effects are
declared with the `effect` keyword; each effect specifies a set of named
operations. A computation that calls an effect operation carries that effect in
its type; the effect is discharged by an enclosing handler. Built-in effects
(`⟪IO⟫`, `⟪State S⟫`, `⟪CompileTime⟫`) follow the same model — they differ
only in that their operations are provided by the runtime or stdlib rather
than user code. The "expose the guts" principle extends to the effect system:
user code can define new effects and handlers with the same expressive power as
the built-ins.

**`⟪Fail E⟫` — parametric failure**: the effect `⟪Fail E⟫` is a parametric
family that unifies the no-information failure effect and the typed-exception
effect:

- `⟪Fail ()⟫` — failure carrying no error value; handled as `Option T`.
  Abbreviated `⟪Fail⟫` where the type parameter is unambiguous.
- `⟪Fail E⟫` for a concrete `E` — failure carrying an error value of type `E`;
  handled as `Result E T`.

This parametric form supersedes and retires `⟪exn⟫` as a distinct effect name.
The Rust `Result` type, Haskell `Maybe`, and Koka-style typed exceptions are all
instances of this one parametric effect, selected by the choice of `E`. The
separation of `⟪Fail E⟫` from `⟪Nondet⟫` is preserved: semidet predicates
compile to simple optional returns, not coroutines.

### 5.1.1 Effect Declarations

Effects are declared at the top level with the `effect` keyword. Each operation
is either **`ctl`** (non-resumable, control-transferring — never returns to the
call site) or **`op`** (resumable — the call site receives a value when the
handler calls the continuation `k`):

```
// Non-resumable: raise transfers control to the nearest enclosing handler
effect [E] Fail
  ctl raise : E → ⊥          // ⊥ is the never type; raise cannot return

// Resumable: the computation continues from after yield when resumed
effect Nondet
  op yield : A → ()

// Resumable: fire-and-forget; computation continues immediately after each write
effect Log
  op write : String → ()

// Resumable: read/write access to a single typed state cell
effect [S] State
  op get : () → S
  op put : S → ()
```

`State` is a **stdlib-defined** algebraic effect — there is no primitive
built-in. Local state is handled by an ordinary `'handle` block that
initialises the cell; for global reactive cells the runtime installs a
system-level handler that wires `get`/`put` to the live data store. Either
way, the user's type signature simply mentions `⟪State S⟫` and `derived`
definitions are transparent (see [SYNTAX.md §9.9](../SYNTAX.md#99-derived-view-definitions)).

Effect operations are **ordinary functions** in scope once the effect is
declared. Calling an operation propagates its effect into the caller's type
automatically — no explicit annotation is required at the call site:

```
safeDivide : Int, Int → ⟪Fail String⟫ Int
safeDivide ← x y →
  y == 0 'when (raise "division by zero")
  x / y
```

The type annotation on `safeDivide` is for documentation; the compiler infers
`⟪Fail String⟫` from the call to `raise`.

### 5.1.2 Effect Handlers

An effect is discharged using `'handle`. For non-resumable effects:

```
computation 'handle
  ctl raise e → Err e        // intercept raise; e is the error value
  return x    → Ok x         // wrap the normal return value
```

For resumable effects, the continuation is an explicit final parameter in
the `op` clause head:

```
computation 'handle
  op yield x k → (accumulator 'append x; k ())   // k is the explicit continuation
  return _     → accumulator
```

**Semantics:**

- **`ctl` clauses** — the clause body is evaluated and becomes the `'handle`
  expression's value. The original computation does not continue. No
  continuation parameter is present.
- **`op` clauses** — the final pattern in the clause head is the explicit
  continuation `k : A → ⟪E2⟫ R`, where `A` is the operation's declared
  return type. Calling `k v` resumes the computation with value `v`.
  Calling `k` zero times terminates that branch (prune); calling it more
  than once forks the computation (as in `⟪Nondet⟫` handlers). `k` is an
  ordinary function value and may be stored, passed, or called from
  anywhere in the clause body.
- **`return x` clause** — maps the computation's normal (non-effect) return
  value to the handler's result type `R`. **Optional**: if omitted, the default
  is `return x → x` (identity), constraining `R = T` where `T` is the
  computation's return type. For `op`-only handlers (logging, tracing, state
  observation) the identity is always correct and the clause may be omitted. For
  `ctl` handlers that change the result type (e.g., wrapping in `Result`), the
  identity default conflicts with the `ctl` clause types and the compiler reports
  a unification error at the handler site, prompting the programmer to add the
  explicit `return` clause.

**Deep handler semantics:** `'handle` installs a handler for the full dynamic
extent of the computation. For resumable `op` clauses, after `resume` is
called and the computation continues, any further operations of the same effect
are caught by the same handler instance — not by an outer handler. The handler
does not need to be re-installed after each `resume`. This is the standard
deep-handler model and is consistent with `'handle` wrapping the entire
computation as an `iter` form.

**Implementation model — two-tier dispatch:**

`ctl` and `op` are compiled differently because their continuation requirements
differ.

**`ctl` — static handler resolution.** At compile time, the whole-program
compiler traces each `ctl` call site to its nearest enclosing `'handle` in the
call graph. When the handler is statically known — the common case, where a
concrete `'handle` block is a fixed number of call frames away — the `ctl` call
compiles to a direct early return. No runtime handler lookup occurs. For
`⟪Fail E⟫`, `ctl raise e` compiles identically to returning `Err e` from a
function whose return type is `Result E a`: the effect is fully erased after
type-checking. The `handleFail` wrapper (above) is a zero-overhead boundary at
the call site.

When the handler is *not* statically known — i.e., the `ctl` call appears inside
a function polymorphic over an effect row, and the enclosing handler is supplied
by the caller — the compiler falls back to **evidence passing**: the function
receives an implicit handler-frame pointer for each effect in its row, and `ctl
raise e` dispatches through that frame. Whole-program specialization eliminates
the evidence parameter whenever the concrete caller is known.

**`op` — evidence passing.** Resumable operations always require a captured
continuation, so evidence passing is used uniformly regardless of whether the
handler is statically known. Each function polymorphic over a resumable effect
carries an implicit evidence parameter. The evidence frame contains the handler
clause as a closure. `op yield x k` looks up the `Nondet` frame, passes `x` to
the handler body, and passes `k` as the explicit continuation. Whole-program
compilation specializes evidence parameters away at concrete call sites,
replacing the frame lookup with a direct call.

**Typing rule:**

```
computation      : ⟪E1 | E2⟫ T      // E1 is the effect being handled
handler clauses  : in context ⟪E2⟫  // clauses may freely perform E2 effects
─────────────────────────────────────────────────────────────────────────────
computation 'handle (...) : ⟪E2⟫ R
```

Clause bodies run in the ambient remaining effect context `⟪E2⟫` — whatever
effects are in the computation's row after `E1` is removed. Clause effects are
inferred normally; no annotation is required. The result row is exactly `⟪E2⟫`.
If a clause body performs an effect not already in `E2`, that effect widens
`E2` and appears in the result row. Partial discharge — handling only `E1` from
a mixed row — falls out automatically: the unhandled effects pass through as
`E2`.

**Expression scope.** `'handle` is an `iter` form and scopes over exactly the
single expression to its left. There is no block form. For multi-step sequences,
the idiomatic pattern is to define a named function whose body contains the
sequence, then apply the handler to the function's result:

```
compute : ⟪Fail String⟫ Int
compute ←
  x ← safeDivide a b
  y ← safeDivide c d
  x + y

compute 'handleFail
```

The function body is already the block; `'handle` wraps the application. This
keeps `'handle` compositional and consistent with LTR, and avoids a second
syntax form.

**Handler reuse — named handlers as functions.** `'handle` is inline-only at
the syntax level. Reuse is achieved through ordinary abstraction: define a
function that wraps an inline `'handle` expression and call it at each use site:

```
handleFail : ⟪Fail E | e⟫ a → ⟪e⟫ (Result E a)
handleFail ← computation →
  computation 'handle
    ctl raise e → Err e
    return x    → Ok x

// use:
safeDivide x y 'handleFail
```

Parameterised handlers are just functions with extra arguments. Handler
composition is function composition. No new type or syntax is required.

**Forward-compatible design (Option C).** A first-class `Handler E1 E2 A R`
type enabling handler values independent of any specific computation is a
possible future extension. The syntax is already forward-compatible: in
[SYNTAX.md §6](../SYNTAX.md#6-projections), `handler_block` is a valid `proj_arg`, so the parser accepts
all four tick section forms for `'handle` — bare-tick `('handle)`,
left-section `(computation 'handle)`, right-section `('handle (...))`,
and the tick-plus-value form. The type checker currently rejects the first
two because handler blocks are not yet runtime values; if Option C is
adopted, only the type checker needs updating. No grammar change is
required.

**Relationship to built-in constructs:** the `'first`, `'any`, `'all`, and
`'collect` consumers (§5.4) are handlers for `⟪Nondet⟫`, defined as wrapper
functions in the stdlib using exactly this pattern. The `⟪Fail E⟫` effect is
equivalently discharged by a handler of the form
`{ ctl raise e → Err e; return x → Ok x }`. `Result` and `Option` are not
primitive; they are the result types of the two canonical `⟪Fail E⟫` handlers.

## 5.2 Effect-Blind Hook Resolution

Effects do **not** participate in hook resolution. Hooks are selected
solely on value types (and dimension types). Once a hook is selected, its
effect annotations are propagated normally through the type system.

This prevents effect annotations from leaking into dispatch in ways that are
hard to reason about, and keeps the hook resolution algorithm effect-agnostic.

**Relationship to `'mapMaybe`, `'flatMap`, and `'traverse`**: these are
distinct iterator names with distinct semantics, not effect-selected variants
of `'map`. `'mapMaybe` is a filter-while-mapping operation (return type
`∃m b[m]`); `'flatMap` is monadic expansion (return type `⟪Nondet⟫ b`).
These differ in what they *do*, not merely in which effect they carry.
Choosing between them is a programmer-level semantic decision: `'map` with a
`⟪Fail⟫` function means "fail the whole mapping if any element fails" —
a different intent from `'mapMaybe`, which silently drops failures. The type
checker enforces that the argument function's effect is compatible with the
chosen iterator's contract, but the choice of name is not automatic.

## 5.3 Effect-Polymorphic Array Operations

### The `Distributable` trait

Not all effects compose uniformly with array mapping. Effects like `⟪IO⟫`,
`⟪State⟫`, and `⟪Fail E⟫` apply one effect per element and collect results
straightforwardly — one output element per input element. `⟪Nondet⟫` is
fundamentally different: a nondeterministic function applied to `n` elements
produces k₁ × k₂ × … × kₙ combinations of results across all elements, not `n`
results. This requires a different combinator.

This distinction is encoded through the **`Distributable` effect trait**:

```
trait Distributable e a b n
  iter [e] 'map a[n], (a → ⟪e⟫ b) → ⟪e⟫ b[n]    // see SYNTAX.md §5.5 for call-vector [e] syntax
```

`⟪IO⟫`, `⟪State⟫`, `⟪Fail E⟫` (including `⟪Fail⟫` = `⟪Fail ()⟫`), pure
(`⟪⟫`), and any effect where one input element produces exactly one output
element implement `Distributable`. `⟪Nondet⟫` does not.

**`Distributable` membership criterion.** `Distributable` is a programmer-declared opt-in:
implement the trait and provide the `iter [e] 'map` hook. The implementation
must satisfy the equational law:

```
(arr 'map f)[i] = arr[i] f     // for all valid i
```

The i-th output element must equal the result of applying `f` to the i-th input
element. This law is not mechanically checked by the type system. In debug
builds the compiler generates property-based tests at the `implementation`
block's compile site; a violation is a **hard compile error**.

**Test mechanics — `Testable` protocol:**

Law testing uses the stdlib `Testable` trait:

```
trait Testable a
  gen 'gen : () → ⟪Nondet⟫ a     // generate sample values of type a
```

Stdlib provides `Testable` instances for all built-in types (`Int`, `Float`,
`String`, `Bool`, `Symbol`, etc.). User-defined types provide their own
`implementation Testable MyType`.

When `implementation Distributable` is compiled in a debug build, the compiler:

1. Samples `n` random arrays using `'gen` for the element type (where `n` is
   the project-level `test_samples` count; default 100; overridable per
   implementation via `/'-MapTest samples:200 -'/`).
2. Tests the law using **identity** as `f` — always available regardless of
   element type.
3. Tests with any additional functions declared via `/'-MapTest f -'/`:
   ```
   /'-MapTest (x → x + 1) -'/
   /'-MapTest (x → x * 2) -'/
   implementation Distributable (MyArray : ⟪State S⟫)
     ...
   ```
4. For each `(arr, f, i)` triple, asserts `(arr 'map f)[i] = arr[i] f`.

**If `a` does not implement `Testable`**: the law cannot be tested; the
compiler emits a **warning** (not an error) and skips the test for that
implementation. The programmer is responsible for verifying the law manually.
This degrades gracefully — the same philosophy as `a[]` for Z3-uncheckable
constraints.

The law cleanly excludes `⟪Nondet⟫`: `f arr[i]` produces multiple values, so
"the i-th element of the output" is undefined and the law cannot hold. It
includes all fail-fast effects: for `⟪Fail E⟫`, if the map succeeds the law
holds element-wise; if any element fails the whole map fails and the law holds
vacuously. `'mapMaybe` (which drops failures and returns a shorter array) is a
separate combinator precisely because it violates this law — its output size
differs from its input size.

### Effect-polymorphic `'map`

`'map` uses a **single effect-polymorphic hook**:

```
iter [e] 'map a[n], (a → ⟪e⟫ b) → ⟪e⟫ b[n] ← prim.vec_map
```

A pure function `a → b` is `a → ⟪⟫ b` (empty effect row). This single
hook covers both pure and effectful cases. Effect rows do **not** need to
participate in specificity ordering — there is only one hook. With whole-
program compilation, the compiler specializes every call site on the concrete
effect, emitting the appropriate code path at compile time:

- **`e = ⟪⟫`** (pure): map is unordered, freely parallelizable, eligible for
  array fusion. The compiler may reorder, vectorize, or eliminate intermediate
  arrays.
- **`e ≠ ⟪⟫`** (effectful, `Distributable` constraint satisfied): map is sequentially
  ordered left-to-right. No parallelization or fusion applied.

No runtime overhead is incurred for either case. The effect type is statically
known at every call site.

### Combinators for non-`Distributable` effects

`⟪Fail⟫` (semidet) and `⟪Nondet⟫` use separate combinators that reflect their
actual semantics:

```
// ⟪Fail⟫: zero or one result per element — filterMap
// returns a dynamic-size array (sigma type)
iter 'mapMaybe a[n], (a → ⟪Fail⟫ b) → ∃(m : Nat, m ≤ n) b[m]

// ⟪Nondet⟫: zero or more results per element — monadic map
// returns a lazy stream of all results across all elements
iter 'flatMap  a[n], (a → ⟪Nondet⟫ b) → ⟪Nondet⟫ b

// ⟪Nondet⟫: if the full matrix of results is needed
iter 'traverse a[n], (a → ⟪Nondet⟫ b) → ⟪Nondet⟫ b[n]
```

This mirrors the Functor / Traversable / Monad hierarchy, expressed through
the effect system and the trait mechanism rather than separate typeclasses.

### Propagation through rank structure

The same principle extends to all rank-polymorphic operations: `'fold`, `'scan`,
`'rank`, etc. are all effect-polymorphic via the same single-hook approach.
Effects propagate outward through the rank structure: a rank-2 operation over a
pure rank-1 operation is pure; over a `Distributable` effectful rank-1 operation it
carries that effect.

## 5.4 Nondeterminism as an Effect

Predicates are **not** a separate type-level concept from functions.
Nondeterminism is modelled as an algebraic **effect**. The determinism
hierarchy decomposes into two axes — can it fail, can it produce multiple
solutions — and maps to two distinct effects:

| Determinism mode | Arra type               | Meaning                              |
|------------------|-------------------------|--------------------------------------|
| `det`        | `a → b`                 | exactly one solution; pure function  |
| `semidet`    | `a → ⟪Fail⟫ b`          | zero or one solution                 |
| `multi`      | `a → ⟪Nondet⟫ b`        | one or more solutions (see below)    |
| `nondet`     | `a → ⟪Nondet⟫ b`        | zero or more solutions               |
| `cc_multi`   | `a → ⟪Nondet⟫ b`        | committed choice; use `'first`       |
| `cc_nondet`  | `a → ⟪Nondet⟫ b`        | committed choice; use `'first`       |
| `erroneous`  | `a → ⟪Fail E⟫ b`        | always raises; `b` is phantom        |
| `failure`    | `a → ⟪Fail⟫ b`          | always fails; a convention           |

`⟪Fail⟫` and `⟪Nondet⟫` are kept as **distinct effects**. `semidet` predicates
are very common (membership tests, lookups, guards) and must compile to simple
optional returns — no coroutine machinery. Collapsing `⟪Fail⟫` into `⟪Nondet⟫`
would impose coroutine overhead on the most common case.

`⟪Nondet⟫` is a user-observable **algebraic effect** (§5.1.1) with a single
resumable operation `yield`. The `'all`, `'first`, `'any`, and `'collect`
consumers (below) are handlers for `⟪Nondet⟫` written in the standard handler
syntax. The pull-based state machine described below is the compilation strategy
for the handler mechanism, not a separate semantic concept.

`cc_multi` and `cc_nondet` (committed-choice) are not separate types. They are
`⟪Nondet⟫` computations where the programmer applies `'first` explicitly,
committing to the first solution.

**The `multi` guarantee and static verification:**

`multi` (at least one solution) and `nondet` (zero or more) share the same type
`a → ⟪Nondet⟫ b` because statically guaranteeing non-emptiness requires a
liveness proof, which is generally undecidable. The guarantee is handled via a
hybrid approach:

1. The compiler uses **Z3 and structural analysis** to attempt a static proof
   that the predicate always produces at least one solution.
2. If the static proof succeeds: no runtime overhead; the guarantee is fully
   verified at compile time.
3. If the static proof fails: the programmer may provide an explicit
   **`/'-Multi-'/` attribute** asserting the guarantee. This inserts a **permanent
   runtime assertion** — the first `next()` on the generated stream must not
   return `Done`, in both debug and release builds. The overhead is a single
   comparison at the first consumption point, which is negligible (the consumer
   was going to call `next()` anyway). The type is therefore always sound:
   violations are caught at runtime, not silently ignored.
4. Without a static proof or `/'-Multi-'/`, the predicate is typed as `nondet` and
   the consumer handles the empty case.

The `/'-Multi-'/` attribute is not an escape hatch that bypasses the type system.
It is a directive: "verify this at the first consumption point." The type never
lies; it either holds by static proof or is enforced by a runtime check.

**Static prover baseline.** The following cases are guaranteed to be verified
statically without requiring `/'-Multi-'/`. Implementations may handle additional
cases; these are the minimum every compliant implementation must cover:

- **Purely deterministic paths**: the predicate body is structurally total —
  all match branches are exhaustive with no `⟪Fail⟫` on any path and every
  code path reaches a yield or return. Every `det` predicate satisfies `multi`
  trivially and is always verified statically.
- **Literal non-empty collections**: iteration over an array literal or any
  array whose size `n` satisfies `n > 0` by Z3 (e.g., a literal `[a; b; c]`,
  or a parameter constrained by `n > 0` in scope). The compiler verifies
  `n > 0` in the same Z3 batch as the surrounding definition.
- **Z3-provable non-empty domains**: the solution domain is characterised
  entirely by LIA constraints that Z3 can prove satisfiable — e.g.,
  `k ← nat where k > 0` (∃ k : Nat, k > 0 is trivially true in LIA).
- **Unconditional branch**: the predicate contains at least one branch with no
  guard and no possibility of failure. Z3 verifies statically that this branch
  is always reachable.

Cases outside the baseline — data-dependent non-emptiness, recursive
predicates, and complex cross-predicate combinations — require `/'-Multi-'/`.

**Lazy stream runtime mechanics:**

The `⟪Nondet⟫` effect is implemented as a **pull-based state machine**. The
computation is compiled to a generator:

- `fail()` terminates the generator (emits `Done`)
- A successful return yields a value (`Yield(v, resume)`)
- Choice points are compiled into state machine transitions: one branch is
  taken immediately; on the next `next()` call, the alternative branch is tried

Backtracking state is heap-allocated as part of the generator object. No stack
copying is required. The GC traces generator objects as ordinary heap values;
all captures at suspension points must be heap-resident (no stack references
across yield points).

Standard consumers:

```
'all   : (a → ⟪Nondet⟫ b) → a → b[]      // collect all solutions
'first : (a → ⟪Nondet⟫ b) → a → Maybe b  // take first; short-circuits
'any   : (a → ⟪Nondet⟫ b) → a → Bool     // does any solution exist?
```

`⟪Fail⟫` (semidet) is handled separately and more cheaply: a `semidet`
predicate compiles to a function returning `Maybe b` — no coroutine, no state
machine, no heap allocation beyond the optional value.

With whole-program compilation, **generator + consumer pairs fuse**: a
`'first (pred arr)` compiles to a simple loop that halts on the first match,
allocating no generator object in the common case. `'any` similarly fuses to an
early-exit loop. `'all` with an otherwise-pure predicate is eligible for array
fusion (see §5.5).

## 5.5 Purity and Optimization

Pure functions (no effects in their type, i.e. `e = ⟪⟫`) are eligible for
aggressive optimization:

- **Array fusion**: see §5.7.
- **Memoization**: pure functions with the same arguments always return the
  same result; the compiler may cache results at call sites with statically
  known arguments.
- **Algebraic rewriting**: pure expressions are subject to algebraic rewriting
  at compile time (see Layer 1 in §5.7).

The effect system makes purity explicit and checkable from the effect row; no
heuristic is required. The whole-program compilation model ensures all
optimization opportunities are visible across module boundaries.

## 5.6 Compile-Time Effect

`⟪CompileTime⟫` is a first-class effect marking computation that executes in
the compiler process rather than at program runtime. It is the required effect
of any function whose result appears in a top-level `.( )` splice (see [07_METAPROG.md §7](07_METAPROG.md#7-quoted-programs-and-compile-time-computation)).

`⟪CompileTime⟫` is **disjoint** from all runtime effects — there is no
implicit lifting and no `liftIO`-style bridge. A `⟪IO⟫` function cannot be
called from a `⟪CompileTime⟫` context; a `⟪CompileTime⟫` function cannot be
called from a `⟪IO⟫` context. The compiler enforces this at the effect-type
level: every sub-expression in a top-level `.( )` splice must be pure (`⟪⟫`)
or `⟪CompileTime⟫`. This keeps compile-time behavior bounded and reproducible
— the set of permitted compile-time side effects is exactly what the stdlib
exposes via `⟪CompileTime⟫` hooks, nothing more.

**Phase separation — runtime cannot call compile-time.** `⟪CompileTime⟫`
functions execute during compilation; the runtime process does not exist at
that point. A runtime function cannot call a compile-time function. Compile-time
results enter runtime code through exactly two mechanisms:
- **`.( )` splicing** — a compile-time expression is evaluated and its result
  is inserted as generated code or a generated constant at the splice site.
- **Inlined constants** — scalar `⟪CompileTime⟫` values that are structurally
  constant (integers, strings, booleans) may be embedded as literals in the
  generated code. The compiler inlines them transparently.

There is no `from ⟪CompileTime⟫ a → a` coercion at runtime; the phase
separation is absolute.

**Compile-time I/O capabilities**: the stdlib provides `⟪CompileTime⟫`
hooks for:
- File system reads (not writes)
- Network reads (HTTP GET, TCP reads)
- Environment variable reads
- Compiler introspection (in-scope names, type of an expression)

These are distinct hooks from their runtime `⟪IO⟫` counterparts. A
function that fetches a database schema over the network at compile time has
type `String → ⟪CompileTime | Fail IOError⟫ Schema`, not
`String → ⟪IO⟫ Schema`.

**Compile-time I/O failure — `⟪Fail IOError⟫`.** Compile-time I/O operations
carry `⟪Fail IOError⟫` in their effect row alongside `⟪CompileTime⟫`. A type
provider's network call can fail; the failure must be handled explicitly at the
splice site using the standard `'handle` mechanism. This is consistent with the
rest of the effect system and makes failure handling visible in source:

```
// stdlib type provider signature:
fetchSchema : String → ⟪CompileTime | Fail IOError⟫ Schema

// splice site handles the failure:
.( fetchSchema "postgres://localhost/mydb"
   'handleFail (err → logCompileError err; []) 'flatMap schemaToDecls )
```

An unhandled `⟪Fail IOError⟫` at a splice site is a compile error — the same
as any unhandled effect. This prevents silent degradation: if a type provider
fails and the programmer has not written a handler, the build fails with a
clear error at the splice site.

**Type providers** use `⟪CompileTime | Fail IOError⟫` I/O to generate type
declarations from external sources:

```
fetchSchema   : String → ⟪CompileTime | Fail IOError⟫ Schema
schemaToDecls : Schema → Program Declaration[]   // pure user function

.( fetchSchema "postgres://localhost/mydb"
   'handleFail (err → logCompileError err; [])
   'flatMap schemaToDecls )
// on success emits: data Trade ← {sym: Symbol, price: Float, ts: Timestamp}
//                   data Order ← {id: OrderId, qty: Int, price: Float}
// on failure: emits no declarations; compile error propagates via unhandled Fail
```

**Caching**: the compiler may cache splice results keyed on the hash of all
`⟪CompileTime⟫` I/O consumed. If the hash is unchanged from the previous
build, the splice is not re-evaluated and the cached declarations are used
directly. A splice annotated `/'-NoCache -'/` is always re-evaluated. This
ensures type provider results are deterministic across incremental builds while
still reflecting schema changes when they occur.

## 5.7 Array Fusion

Array fusion eliminates intermediate arrays created by chaining operations. The
**precondition is purity**: an operation is fusion-eligible exactly when its
effect row is empty (`e = ⟪⟫`), which is statically known at every call site
after monomorphization.

The whole-program compilation model means fusion runs after all dispatch is
resolved and all effects are concrete. There is no fragility from unresolved
hooks or module boundaries.

Fusion is organized into three layers applied in sequence.

### Layer 1 — Algebraic Rewriting

Known array laws are applied as unconditional term rewrites at the IR level,
before any structural analysis:

```
// Map fusion: two consecutive maps collapse into one
arr 'map g 'map f         =  arr 'map (f ∘ g)

// Fold-map fusion: inline the map into the fold's combining function
arr 'map f 'fold ⊕ z      =  arr 'fold (a b → ⟦a; b f⟧ ⊕) z

// Scan-map fusion: inline the map into the scan's combining function
arr 'map f 'scan ⊕        =  arr 'scan (a b → ⟦a; b f⟧ ⊕)
```

These follow from first principles — they are algebraically provable from the
semantics of the operations, not discovered empirically. The full Layer 1 rule
set, including filter-related rewrites:

```
// Map fusion
arr 'map g 'map f              =  arr 'map (f ∘ g)

// Filter fusion
arr 'filter p 'filter q        =  arr 'filter (x → p x && q x)

// Map-filter fusion (map before filter: inline map into predicate)
arr 'map f 'filter p           =  arr 'mapMaybe (x → if p (f x) then Some (f x) else None)

// mapMaybe: map-into-passing-branch (Layer 1 algebraic; no intermediate)
arr 'mapMaybe g 'map f         =  arr 'mapMaybe (x → g x 'map f)

// Fold-map fusion: inline map into fold's step
arr 'map f 'fold ⊕ z           =  arr 'fold (acc x → acc ⊕ (x f)) z

// Fold-filter fusion: guard the accumulation step
arr 'filter p 'fold ⊕ z        =  arr 'fold (acc x → if p x then acc ⊕ x else acc) z

// Scan-map fusion: inline map into scan's step
arr 'map f 'scan ⊕             =  arr 'scan (acc x → acc ⊕ (x f))

// flatMap fusion
arr 'flatMap g 'map f          =  arr 'flatMap (x → g x 'map f)
arr 'map f 'flatMap g          =  arr 'flatMap (x → x f g)
```

These are always correct, require no structural analysis, and apply regardless
of rank or size. Array identities (distributivity of reduction over arithmetic,
associativity of append, etc.) are also applied here.

### Layer 2 — SOAC Fusion

Post-monomorphization, a fusion pass classifies pure array operations by their
streaming role and merges adjacent eligible operations.

**SOAC taxonomy:**

| Category | Operations | Vertical fusion role |
|---|---|---|
| Map-like (transformer) | `'map`, `'filter`, `'mapMaybe`, `'flatMap`, `'scan`, `'zipWith` | Fuse freely upstream and downstream; no materialization |
| Reduce-like (consumer) | `'fold`, `'sum`, `'product`, `'any`, `'all`, `'count` | Terminates a vertical chain; fuses with one upstream producer |
| Materialize-required | `'sort`, `'groupby`, `'asofJoin`, `'distinct`, `'rank` (runtime `r`) | Fusion boundary; upstream chain materialises before this op |

**Fusion eligibility condition**: an operation is fusion-eligible iff (a) its
effect row is empty (`e = ⟪⟫`) — checked after monomorphization — and (b) it
is in the map-like or reduce-like category. This is statically decidable.

**Vertical fusion**: a chain of fusion-eligible operations is compiled to a
single loop. No intermediate arrays are allocated. The chain may contain any
sequence of map-like operations followed by at most one reduce-like consumer.
Example: `arr 'filter pred 'map f 'fold (+) 0` — single loop, one pass,
no allocations.

**The sigma type does not force materialization.** `'filter` returns
`∃(m ≤ n) a[m]` — the sigma type describes the *type* of the output, not
whether the output is allocated. The streaming pass checks the predicate and
emits elements lazily; the downstream `'map` or `'fold` consumes them in the
same loop. Materialization only occurs at a **materialize-required** operation
(such as `'sort`), which demands a complete concrete array to operate on.

**Horizontal fusion — multiple pure folds over the same source:**

When two or more reduce-like operations consume the same input in the same
expression, they are fused into a single pass with a tuple accumulator:

```
arr 'fold (+) 0               // r1
arr 'fold (*) 1               // r2 (same arr)
```
→ fused: `arr 'fold (acc x → (acc.1 + x, acc.2 * x)) (0, 1)` — projects to
`(r1, r2)`. Generalises to `n` folds; the accumulator is an `n`-tuple.

This holds whenever all consumers are reduce-like (single-pass) and pure. For
mixed consumers (e.g., one fold and one sort), the sort is a materialization
boundary; the fold fuses with the shared source independently.

**Deferred — rank-polymorphic horizontal fusion and `'scan` horizontal fusion:**
the interaction between nested-rank operations and the horizontal fusion graph,
and the correct treatment of multiple `'scan` consumers sharing an input
(which requires careful dependency analysis because each step of `'scan` depends
on the previous), are deferred to implementation experience.

### Layer 3 — Generator-Consumer Fusion

`⟪Nondet⟫` generators fuse with short-circuiting consumers:

- `'first (pred arr)` — fuses to an early-exit loop; no generator object is
  allocated; the computation halts on the first match.
- `'any (pred arr)` — same; halts on first success.
- `'all (pred arr)` with a pure predicate — treated as a SOAC (Layer 2).

The whole-program compiler determines the consumer statically. A generator
object is only allocated if the computation genuinely suspends and resumes, which
`'first` and `'any` never do.
