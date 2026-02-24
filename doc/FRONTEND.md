# Arra — Frontend

The frontend receives source text and produces Core IR. It is implemented
entirely in OCaml and is shared by both backends. The pipeline:

```
Source text
  └─ Lexer / Parser              → untyped AST
       └─ Type Checker           → typed AST
            ├─ Keyword-first / kind-first disambiguation
            ├─ Type inference
            ├─ Fork detection rewrite
            ├─ Hook resolution
            ├─ Definition-site ambiguity check
            └─ Z3 dimension checking
                 └─ Program T Expansion  → typed AST (splice-free)
                      └─ Core Lowering   → Core IR
```

---

## 1. Lexer / Parser

Produces an untyped AST from source text. No semantic analysis at this stage.

**Layout rule.** Indentation-sensitive block structure following a Haskell-style
layout rule: a new definition or clause at the same indentation level as the
previous one continues the enclosing block; a deeper indent opens a nested block.
The exact rule is specified in SYNTAX.md.

**Unicode / ASCII duality.** Every Unicode operator or keyword has an ASCII
equivalent accepted interchangeably. Both forms are valid in source; a file
watcher can round-trip ASCII to Unicode on save (Uiua-style). The lexer
normalises to the Unicode form internally.

**Attribute parsing.** Compile-time attributes (`/'-Attr-'/`) are lexed as
structured tokens and attached to the AST node that follows them. Attributes
carry free-text payloads (`/'-Z3Budget 5000-'/`, `/'-CMonoid-'/`, etc.) that are
parsed by later passes.

**`iso_def` block form.** The `iso` block form (two `from` hooks declared
together with round-trip laws) is handled at parse time as a single AST node
that the type checker expands.

---

## 2. Type Checker

A pipeline of sub-passes over the untyped AST, producing a fully-annotated
typed AST where every node carries its type. All passes run over the same AST
in sequence; each produces annotations consumed by later passes.

### 2.1 Keyword-First / Kind-First Disambiguation

Determines which hook keyword pool applies at each dispatch site before hook
resolution runs.

- **`uop`** — distinguished by having no right argument: `val sym`. The operator follows its operand with nothing after it.
- **Infix symbolic forms** — the right argument determines the pool:
  - Lambda expression (syntactically a function literal) → `iter` / `gen`
  - Named reference with function type → `iter` / `gen`
  - Named reference with value type → `op` (binary value operator)
  - Genuinely ambiguous (unconstrained type variable) → annotation required
- **`project` / `assign`** — distinguished by their field/index argument form.
- **`app`** — fires in call position; `pat` fires in pattern position.
- **`from` / `gen`** — fire when the result type drives resolution (output
  dispatch; see §2.5).

This pass runs at the definition site, before hook resolution, and produces a
keyword label on every dispatch node.

### 2.2 Type Inference

Bidirectional inference over AST nodes. Each node is either checked against a
known type (inherited from the surrounding context) or synthesised (type flows
outward). The two directions alternate at the boundaries specified by SYNTAX.md.

**Effect rows.** Function types carry effect rows `a → ⟪e⟫ b`. Effect variables
unify like type variables. Effect polymorphism (`∀ e ⇒`) is inferred for
functions whose body contains no concrete effect and whose callers provide one.

**Trait constraints.** Constrained polymorphism `∀ a b ⇒ …` is inferred when
a hook application appears in a polymorphic context. Constraints accumulate
during inference and are discharged at call sites during specialization. No
runtime dictionaries are generated — hooks are resolved to direct references
before Core.

**Sigma types.** When a sigma-typed expression is eliminated (pattern-bound or
let-bound), the compiler:
1. Binds the size component to a fresh term-level variable (e.g. `m : Nat`).
2. Discards the proof component but adds its content to the current
   definition's Z3 batch as an explicit hypothesis.
3. Types the array component via substitution: `a[m]`.
In the body, `m` is an ordinary value and Z3 knows the constraint.

**Row types.** Inference maintains a **lacks constraint** on row variables:
`{f: T | rest}` implicitly constrains `rest` to lack field `f`. The constraint
propagates through inference without new syntax. Rows are normalised to
canonical lexicographic field order at each unification step.

**`from` / `gen` output dispatch.** When the result type of an expression is
known from context (annotation, downstream inference), `from` and `gen` hooks
are resolved on that result type. When the result type is still unknown, the
compiler defers resolution; if it remains unknown at the end of the definition,
a type ambiguity error is reported.

### 2.3 Fork Detection Rewrite

Runs after type inference, before hook dispatch. When both operands of a
symbolic form have fully-known function types with a **common domain**, the
expression is rewritten to an `liftA2` application (a fork):

```
// Before: f op g  where f : a → b, g : a → c, domain a unifies
// After:  liftA2 op_section f g  : a → d
```

**Detection is eager**: fires only when both operand types are fully known. If
either type is still a variable, fork detection is skipped and hook dispatch
proceeds on the raw `sym_proj` node.

**Recursion**: `f + g + h` parses as `(f + g) + h`. Fork fires on the inner
pair first, producing a new function; fork detection then fires again on the
result and `h`.

**Opt-out**: `⟦f; g⟧ op` (call-vector form) bypasses `sym_proj` entirely; fork
detection never fires.

**Diagnostic**: when operand types cannot be determined and a fork may have been
intended, the compiler emits a targeted message suggesting a type annotation or
explicit `liftA2`.

Fork detection is eager and correct by construction — it fires when both
operand types are known (fork rewrite applied), and is skipped when they are
not (falls through to `bop` hook dispatch). The fallthrough case produces a
`bop` resolution failure if the operands turn out to be functions. To make
this actionable, after any `bop` resolution failure the compiler checks
post-hoc whether both operands have function types with a common domain; if
so, it appends a note to the error:

```
error: no bop (+) hook for types (a → b) and (a → c)
note: if fork was intended, annotate the expression with its result type
      or use explicit liftA2
```

This requires no semantic change — it is a one-pass heuristic check after
failure, not a second fork detection attempt. The pathological case (both
operand types still unconstrained after full type inference) always has the
same fix: add a type annotation to the binding.

**Trains are fully subsumed by this pass.** Atop (2-train: `f g` where both
are functions) falls out of the quasi-concatenative grammar and the
`Functor (a →)` instance — no compiler pass required. Forks (3-train) are
this pass. Tick modifier sections (§4.3 of semantics/04_TRAINS.md) are
first-class functions that compose via `∘`; no special handling is needed.
No separate train rewriting pass exists or is needed.

### 2.4 Hook Resolution

Applies the keyword label from §2.1 and selects a single hook at every dispatch
site using the containing module's scope only. Whole-program pass — all
definitions are visible simultaneously.

**Specificity ordering.** Among candidates, the most specific pattern wins:

```
literal value pattern              (1 : Int)       // most specific
concrete type + literal dim        Int[3]
concrete type + dim variable       Int[n]
concrete type + dyn dim            Int[]
concrete type, no dim              Int
trait-constrained + dim            (a : Num)[n]
trait-constrained                  (a : Num)
unconstrained + dim                a[n]
unconstrained                      a               // least specific
```

Cross-position variable sharing is a tiebreaker: `(a[n], a[n])` is more
specific than `(a[n], b[m])`. Function types decompose over domain and codomain;
array ranks are not subtype-related (`a[n]` and `a[n;m]` are incomparable).

For `from` and `gen`, the result type is the primary specificity dimension;
source type / seed type is secondary.

**Import-boundary finalization.** Each call site resolves hooks using only the
hooks visible at the *containing module* — its own definitions plus its direct
imports. A downstream module cannot retroactively affect an upstream module's
internal call sites. Hook sets are finalized at the import boundary.

**Error on ambiguity.** If no single most-specific hook exists after the
keyword-first pass, it is a compile error naming both conflicting hooks and a
witness type that would produce the ambiguous call.

**`pat` hook exhaustiveness and disjointness.** `pat` hook branches must be
pairwise disjoint and collectively exhaustive. Verification uses two tiers
composed in sequence:

1. **Structural tier.** Constructor coverage is checked first: every constructor
   of the matched ADT must appear in at least one branch. Purely syntactic — no
   Z3 involvement.
2. **Z3 tier.** For each constructor, `when`-guard coverage is checked: the
   disjunction of all guards for that constructor must cover the constructor's
   full domain. Z3 decides this using LIA constraints from the guards and the
   hooks' return-type sigma constraints.

The tiers are composed, not independent: structural gaps are errors before Z3
runs; Z3 guard gaps are errors per-constructor after the structural check passes.
A match expression is `⟪Fail⟫`-free only when both tiers succeed.

```
// Two pat hooks for Circle, leaving r = 0.0 uncovered:
//   pat (Circle r : Shape) when r > 0.0 → PositiveCircle
//   pat (Circle r : Shape) when r < 0.0 → NegativeCircle

my_shape (PositiveCircle → process_pos; NegativeCircle → process_neg)
// error: pat hooks for (Circle r : Shape) do not cover r = 0.0
//   note: Z3 finds ¬(r > 0.0) ∧ ¬(r < 0.0) is satisfiable

// With guard gap fixed (r ≥ 0.0 and r < 0.0, plus r = 0.0 case):
//   pat (Circle r : Shape) when r > 0.0 → PosCircle
//   pat (Circle r : Shape) when r = 0.0 → ZeroCircle
//   pat (Circle r : Shape) when r < 0.0 → NegCircle

my_shape (PosCircle → ...; ZeroCircle → ...; NegCircle → ...)
// Z3 verifies: (r > 0) ∨ (r = 0) ∨ (r < 0) covers all of Float ✓
```

**Non-LIA fallback.** When a `pat` hook's guard is outside the LIA fragment
(e.g., nonlinear arithmetic, bit-vector constraints), Z3 cannot decide coverage.
Resolution: the branch is accepted with a warning and semantics fall back to
first-match-in-definition-order. The programmer is responsible for disjointness.

**Recursive `pat` hooks.** When a hook invokes itself on sub-terms, the
structural tier handles constructor disjointness as before (constructors are
syntactically distinct). For inductive arithmetic — hooks that peel one layer of
a recursive numeric type — Z3 cannot prove guard coverage by induction; this
falls through to the non-LIA fallback (warning + first-match semantics).

### 2.5 Definition-Site Ambiguity Check

When a new hook `O_new` is added, it is checked pairwise against every existing
hook for the same operator. Two steps:

**Step 1 — Incomparability.** If one hook is strictly more specific than the
other, no conflict exists. Only incomparable pairs proceed to step 2.

**Step 2 — Pattern intersection.** Pattern unification applied component-wise
determines whether a non-empty intersection exists:

| Unification case | Result |
|---|---|
| `unify(T, T)` | success |
| `unify(T1, T2)`, `T1 ≠ T2` (concrete) | fail — disjoint |
| `unify(a, T)` | `{a = T}` |
| `unify(a : C, T)` | `{a = T}` if `T : C`, else fail |
| `unify(a : C1, b : C2)` | success if `∃ τ. τ : C1 ∧ τ : C2` |
| `unify(T[k], T[n])` (`n` variable) | `{n = k}` |
| `unify(T[k1], T[k2])`, `k1 ≠ k2` | fail |
| `unify(T[], T[n])` | success |
| `unify(a[n], a[n;m])` | fail — rank mismatch |
| `unify({f₁:T₁…}, {f₁:U₁…})` | recurse; fail if field sets differ |
| `unify((a,b), (a,b,c))` | fail — arity mismatch |
| Full signature | conjunction of component results |

If both steps succeed, `O_new` is rejected at the definition site with an error
naming both conflicting hooks and a witness type.

The shared-witness check `∃ τ. τ : C1 ∧ τ : C2` is decidable under
whole-program compilation.

### 2.6 Z3 Dimension Checking

Runs once per top-level definition, after type inference has collected all
dimension constraints for that definition.

**Invocation model — batch per definition.** All constraints from a single
top-level definition are submitted to Z3 in one call:
- Background axioms: `v ≥ 0` for every dimension variable `v` in scope
- Constraints from the type signature and hook heads
- Constraints from sigma type eliminations in the body
- Guard conditions from `when` clauses (linear arithmetic subset only)

**Dimension variable freshness.** Before submission, every dimension variable
is alpha-renamed to a globally unique identifier encoding its definition site
(e.g. `n` in `f` becomes `f$n`). Name collisions across definitions are
structurally impossible.

**Isolated Z3 context.** Each batch runs in an isolated context — no solver
state leaks between definitions. Results are content-addressed cached; unchanged
definitions reuse cached results without invoking Z3.

**UNSAT — type error.** The compiler extracts a minimal UNSAT core via
`get_unsat_core` and maps each assertion back to its source location using a
constraint provenance table built during inference:

```
type error: contradictory size constraints in `reshape2d`
  (1)  m ≤ n   — from sigma elimination at reshape2d.arra:14:8
  (2)  m > n   — from when-guard at reshape2d.arra:21:3
  constraints (1) and (2) cannot both hold
```

The provenance table is discarded after the error is reported (not cached).

**Timeout policy.** Z3 has a configurable per-definition step budget (not wall
time — step counts are reproducible). On timeout:
- Hard error at the failing site.
- The constraint is internally degraded to `a[]` (dynamic size) for the
  remainder of the type-check pass, so all remaining errors in the module are
  still reported in a single pass.
- The programmer may raise the budget with `/'-Z3Budget N-'/` on the definition,
  refactor the constraint, or explicitly annotate `a[]` to opt out.

**Z3 version pinning.** Solver step counts are not stable across Z3 versions.
The project's Z3 version is specified in the project manifest as a hard build
dependency.

**Cross-module boundaries.** Each function signature is verified once against
its body. Callers use the signature as a trusted discharged lemma without
looking into the body. When a caller eliminates a sigma type from a dependency
module, the proof content is added as an explicit hypothesis to the caller's
local Z3 batch — no cross-module Z3 query is required.

**Module boundary**: All Z3 interaction is hidden behind a single OCaml module
boundary (`val check : constraints -> result`). Nothing outside that module
calls Z3 directly.

---

## 3. Program T Expansion

Evaluates compile-time splices in a fixed-point loop. Each iteration is three
passes:

**Pass 1 — Type-check non-splice definitions.** Every value binding, hook,
`type` definition, and trait implementation that does not contain a top-level
splice is type-checked. Splice functions (functions whose results will be
spliced) are also type-checked here. The return type of every splice
(`Program T` or `Program Declaration`) is known before any splice is evaluated.

**Pass 2 — Evaluate top-level splices in source order.** Each `.( expr )` at
module scope is evaluated. `expr` must be pure or `⟪CompileTime | Fail IOError⟫`.
The resulting `Program T` or `Program Declaration[]` values are inserted at the
splice site.

**Pass 3 — Type-check generated declarations.** Declarations produced by
splices are type-checked. If any generated declaration contains a further
top-level splice, return to Pass 2. A cycle is a compile error.

**Hook resolution at quote time.** Hooks embedded in `` `[expr] `` are resolved
at the quotation site (Pass 1), not at splice time. The splice produces typed
AST; no re-resolution is needed in Pass 3. Consequence: if the splice site has
a more-specific hook for the same operator, the embedded hook is used unchanged.
This is correct — the library's semantics are preserved — and always type-safe.

**No stage restriction.** Whole-program compilation makes all definitions
visible in Pass 1. Splice functions may be defined in the same module as their
use sites.

**`gensym` for hygiene.** Introduced names (lambda parameters, let-bindings) in
quoted forms carry no automatic hygiene. The stdlib provides:
```
gensym : String → ⟪CompileTime⟫ Name
```
Freshness is global within a single compilation — no two `gensym` calls produce
the same name regardless of call site.

**Splice result caching.** The compiler may cache splice results keyed on the
hash of all `⟪CompileTime⟫` I/O consumed. A splice annotated `/'-NoCache-'/` is
always re-evaluated.

**Re-entrant snapshot invariant.** Each splice evaluation produces an **atomic
batch**: the splice's output declarations plus all sub-declarations from nested
splices form one unit. Either the entire batch commits or none of it does. The
Pass 1 snapshot is the rollback base for any batch failure — intermediate pass
state within a batch is never externally visible. This generalises to arbitrary
nesting: a failed splice at depth N rolls back all batches at depth ≥ N,
returning to the last committed state (Pass 1, or a previously committed
shallower batch).

A splice-bearing declaration produced by an outer splice is not committed
independently of the inner splice it contains — the outer and inner outputs
are part of the same batch. Rolling back only the inner output would leave a
dangling splice-bearing declaration with no output, which is semantically
broken.

```
// Pass 1 snapshot S1: {gen_outer, gen_inner} type-checked

// Pass 2: evaluate .( gen_outer () )
//   produces: {stub ← .( gen_inner() ), helper : Int → Int, helper x ← x+1}

// Pass 3: type-check batch — stub well-typed; helper well-typed
//   inner splice found in stub → hold batch pending; return to Pass 2

// Pass 2: evaluate .( gen_inner () )
//   gen_inner() raises a runtime error

// → inner batch fails → pending outer batch also discarded (atomic)
// → rollback to S1; compile error at the outer splice site

// If gen_inner() succeeds and produces {foo : Int, foo ← 42}:
// → Pass 3: type-check {foo, foo-body} ✓
// → commit: S2 = S1 + {stub, helper, helper-body, foo, foo-body}
```

---

## 4. Core Lowering

Desugars the splice-free typed AST to Core IR. Core is fully explicit — no
sugar, no implicit coercions, all dispatch resolved to direct references.

| Surface | Core |
|---|---|
| `r.x` | `project` hook dispatch → `ProjF r x` |
| `r.(x ← v)` | `assign` hook dispatch → `Prim prim.rec_set` |
| `t.1` | `ProjI t 1` (structural, no hook) |
| `view` | `project` dispatch call marked read-only |
| `iso A B (…)` | two `from` hooks + round-trip law registration with `Testable` |
| `'handle` | explicit `Handle` node; `ctl` → static early-return path; `op` → evidence-passing continuation |
| `derived` | dependency analysis over `⟪State⟫` cells; delta function stubs registered |
| Hook calls | direct references to resolved monomorphic implementations |
| `from Field → function` | explicit accessor calls via `prim.field_to_fn` |
| `distinct Foo ← T` | `Prim prim.unsafe_coerce` at wrap / unwrap sites |

Core IR is the QCheck boundary. Both backends consume identical Core; any
divergence in output is a backend bug. See CORE.md for the full Core IR
specification.

---

## 5. Module System

### 5.1 Structure

Each module is a single file with two optional sub-blocks:

```
module Name
  interface
    // public declarations visible to importers
  implementation
    // private definitions
```

A name declared in `interface` (via `name : Type`) must be defined in
`implementation`. The compiler verifies completeness at the module boundary.

**Interface block** accepts: type signatures, `type`, `distinct`, `alias`,
`effect`, `trait`, `derived` (name + type only, exported as read-only), and
`open` re-exports.

**Implementation block** accepts: value bindings, hook definitions, `derived`
full expressions, `implementation` blocks, and private helpers.

### 5.2 Imports and Coherence

`open Module` — in the interface block, re-exports `Module`'s public names.
In the implementation block, makes them available privately.

Qualified access `Module.name` is always available regardless of `open`
statements. Circular imports are forbidden — the dependency graph must be a DAG.

### 5.3 Hook Finalization at the Import Boundary

Each call site resolves hooks using only the scope of the containing module.
A downstream module B cannot retroactively affect module A's internal call
sites, even if B defines a more-specific hook. A's behaviour is determined
entirely by what A imports. Whole-program compilation uses this guarantee to
resolve all hooks statically — no runtime dispatch tables exist.

### 5.4 Orphan Rule

A hook or trait implementation is only valid if the defining module owns
**either** the operator/trait **or** at least one of the types involved. To
implement an external trait for an external type, wrap the external type in a
`distinct` — this is the newtype wrapper pattern. `distinct` types are
structurally disjoint from the wrapped type and never conflict with its hooks.

### 5.5 Abstract Types

A data type is **transparent** if its constructors are exported (full
definition in the interface); **abstract** if only the type name is exported
(definition in the implementation). Abstract types are the primary data-hiding
mechanism; public API is exposed through type signatures and hooks in the
interface block.

---

## 6. Incremental Type Checker State

Required for both the REPL and the LSP server. The design is shared between
both consumers and should be done carefully from the start.

**Snapshot model.** Each committed expression or definition produces a
**type checker snapshot** — a serialisable representation of the environment
at that point, including:
- All bindings and their types
- All hooks in scope and their resolved targets
- All trait implementations in scope
- Z3-cached dimension results for all definitions committed so far

**Rollback.** On error, the snapshot rolls back to the last successful state.
The session is never left in a partially-committed state.

**Module imports at the REPL prompt** extend the snapshot with the imported
module's public names, resolved hooks, and Z3-cached dimension results.

**REPL use.** The treewalk interpreter plugs in after Core Lowering and
interprets directly. Each expression is parsed, type-checked against the current
snapshot, lowered to Core IR, and interpreted. The snapshot is extended on
success, rolled back on error.

**LSP use.** `arra lsp` starts the compiler in LSP server mode, communicating
with the editor via stdin/stdout JSON-RPC. The snapshot lives in that process's
heap — there is no snapshot IPC. The editor sends text change events and
receives diagnostics, hover types, and completions as JSON-RPC responses.
Keystroke-level re-checking starts from the last valid snapshot for the changed
definition. The snapshot model is the performance-critical path for editor
responsiveness.

**Watch mode.** `arra watch` provides a lighter `cargo watch`-style alternative:
file watcher triggers a full incremental re-check on save, printing errors to
the terminal. Shares the same incremental infrastructure as the LSP; not a
substitute for users who need hover types, go-to-definition, or inline Z3 error
explanations.

**Re-entrant type checker for Program T expansion.** The type checker must be
re-entrant during Program T expansion: evaluating a splice may produce new
declarations that require type-checking, which may produce further splices. The
snapshot mechanism provides natural re-entrancy — each pass operates on a
consistent snapshot extended with the declarations produced so far.

**Serialization format: S-expressions** via `ppx_sexp_conv`. Portable across
OCaml versions and machine architectures, human-readable, inspectable when
debugging. `[@@deriving sexp]` derives serializers automatically. OCaml Marshal
is rejected: its binary format is version- and architecture-specific, breaking
distributed build caches on compiler upgrades. Custom binary is deferred unless
profiling shows S-expression parsing is a bottleneck.

**Z3 results: content-addressed separately.** Z3 results are stored in a cache
keyed by `(AST hash of definition, Z3 version string)` and referenced from the
main snapshot by hash. A Z3 version bump invalidates only the Z3 cache; type
binding snapshots remain valid. Shared definitions (e.g. stdlib) share cache
entries across projects — a CI run pre-populates the Z3 cache for downstream
builds. Definition hash is the AST hash (parsed, not type-checked): more stable
than source text hash, computable before type-checking.

**Invalidation: per-definition with eager transitive closure.** When a
definition changes, the full transitive closure of dependents in the definition
dependency DAG is invalidated upfront. Correct by construction; simpler than
lazy partial invalidation. The transitive closure of a typical change is small;
the expensive case (widely-imported stdlib change) is rare and expected to be
slow. Batch compilation uses per-module granularity (simpler; consistent with
semantics/01_EXECUTION.md §1.4); the snapshot machinery exposes both.

**Dependency DAG:** direct syntactic dependencies at the definition level —
names appearing free in the definition body. The DAG is built during
type-checking and updated incrementally on each re-check.


## Open Questions
