# Arra — Roadmap

## Architecture Overview

The implementation splits into a shared frontend, a shared Core IR, and two independent backends. The split point is Core IR: everything above it is implemented once in OCaml; everything below diverges.

```
Source
  └─ Lexer / Parser                           (OCaml)
       └─ Type Checker                        (OCaml)
            ├─ Keyword-first / kind-first
            ├─ Type inference
            ├─ Fork detection rewrite
            ├─ Hook resolution
            ├─ Definition-site ambiguity check
            └─ Z3 dimension checking          (OCaml, Z3 bindings)
                 └─ Program T Expansion       (OCaml, fixed-point)
                      └─ Core Lowering        (OCaml)
                           ├─ Treewalk Interpreter         (OCaml 5 effects)
                           └─ Whole-Program Passes         (OCaml)
                                ├─ Specialization / monomorphization
                                ├─ Effect-based specialization
                                ├─ Layer 1 — algebraic rewriting
                                ├─ Layer 2 — SOAC fusion
                                ├─ Layer 3 — generator-consumer fusion
                                └─ Derived DAG construction
                                     └─ ANF / Closure Conversion    (OCaml)
                                          └─ C Emission              (OCaml → C)
                                               └─ Primitive Kernels  (C + inline assembly)
```

---

## Phase 1a — Layout Preprocessor

The layout rule must be resolved before the token stream is fed to the Menhir
grammar. A dedicated preprocessor pass runs between the raw lexer and the parser,
inserting virtual `{`, `;`, and `}` tokens according to the indentation rules in
`SYNTAX.md §1.7`. This pass is deliberately isolated so it can be tested
independently from the grammar.

Non-trivial cases to cover in the preprocessor test suite before building the
parser on top:

- `iso` blocks and their nested indentation
- `'handle` blocks with multiple clauses on separate lines
- Same-line `(` suppressing layout-block opening
- Attribute-scope blocks `attr '(' decl* ')'` which are not layout-governed

See [FRONTEND.md §1](./FRONTEND.md) for the full layout rule specification.

---

## Phase 1b — Lexer / Menhir Grammar

Builds on the preprocessor token stream. Produces an untyped `Surface.Ast`
from source text. Handles Unicode/ASCII duality, attribute parsing, and the
`iso_def` block form. No semantic analysis.

Two grammar features need extra care before writing Menhir rules:

- **Argument-then-function juxtaposition** — `f g h` = `h(g(f))`. Juxtaposition
  associates left but the *rightmost* element is the function, which is the
  reverse of standard ML-style application. Left-recursive expression rules in
  Menhir need adjustment for this call-direction convention.
- **Result-type dispatch hooks** (`gen`, `from`, `iso`) — these appear inside
  expression position and are annotated differently in the AST from the
  input-type dispatch hooks. The grammar must distinguish them syntactically so
  the type checker receives the right `hook_head` variant.

See [FRONTEND.md §1](./FRONTEND.md) for full design.

---

## Phase 2 — Type Checker Architecture Sketch

The type checker is the most complex single piece of the frontend. Its
architecture must be designed before implementation begins because several
decisions have wide-ranging consequences:

- **Incremental snapshot format** — each committed definition produces a
  serialisable snapshot of the typing environment. The snapshot is shared with
  the REPL and LSP server. The data structures chosen here affect what the TC
  can and cannot do incrementally, which in turn affects the REPL latency
  profile and LSP responsiveness. Decide the snapshot format before writing
  any inference code.
- **Hook specificity algorithm** — the static resolution ordering
  (keyword-first / kind-first / import-boundary finalization) must be fully
  specified as a decision procedure before implementation, because it is the
  primary novelty of the language and errors here are hard to fix post-hoc.
- **Fork detection rewrite** — the pass that rewrites guarded expressions into
  disjoint case branches (Z3-verified) is a pre-pass over the typed AST.
  Its interaction with bidirectional inference needs to be sketched in the
  architecture document before the TC is built, even if this pass is
  implemented last.
- **Re-entrant TC for Program T** — the macro expansion fixed-point requires
  the TC to be incrementally re-entrant. Sketch this interface even if Program
  T expansion is implemented later; retrofitting re-entrancy is expensive.

This phase produces an architecture document (not code). It runs in parallel
with Phase 1b and should be complete before Phase 3 begins.

---

## Phase 3 — Type Checker

A pipeline of sub-passes: keyword-first/kind-first disambiguation, bidirectional
type inference, fork detection rewrite, hook resolution with specificity ordering
and import-boundary finalization, definition-site ambiguity checking, and Z3
dimension checking batched per definition.

See [FRONTEND.md §2](./FRONTEND.md) for full design.

---

## Phase 4 — Program T Expansion

Evaluates compile-time splices in a three-pass fixed-point loop. Hooks are
resolved at quote time; the splice produces typed AST; no re-resolution at
splice time. A cycle is a compile error.

See [FRONTEND.md §3](./FRONTEND.md) for full design.

---

## Phase 5 — Core Lowering

Desugars the typed AST to Core IR — no sugar, no implicit coercions, all
dispatch resolved to direct references. Core IR is the QCheck boundary between
the frontend and both backends.

Start with simple fragments — plain lambdas, let bindings, basic case — and
wire them end-to-end through the pipeline (parser → TC → lowering → treewalk)
as early as possible. Validating the round-trip on a small fragment before the
full hook surface is implemented catches architectural mismatches cheaply.
Expand incrementally to the full hook surface: project/assign sugar, effect
handle/perform elaboration, iso desugaring to two `from` hooks.

See [FRONTEND.md §4](./FRONTEND.md) and [CORE.md](./CORE.md) for full design.

---

## Phase 6 — Treewalk Backend

Interprets Core IR directly using OCaml 5's native effect system. Naively correct,
zero optimization. The QCheck oracle for all compiler passes.

See [BACKEND.md §1](./BACKEND.md) for full design.

---

## Phase 7 — Whole-Program Passes (Compiler only)

Specialization / monomorphization, effect-based specialization, Layer 1–3 fusion,
derived DAG construction, and kernel selection. All run over Core IR before C
emission.

See [BACKEND.md §2](./BACKEND.md) for full design.

---

## Phase 8 — ANF / Closure Conversion (Compiler only)

ANF puts Core into administrative normal form; closure conversion converts lambdas
to explicit closure records. Both are required before C emission.

See [BACKEND.md §4](./BACKEND.md) for full design.

---

## Phase 9 — C Emission (Compiler only)

Emits C for control flow, closures, and effect handlers. Hot array primitives are
not emitted as C loops — kernel selection (Phase 6) has already matched fused
sequences to hand-written kernels; the emitted C calls into them directly.

See [BACKEND.md §5](./BACKEND.md) for full design.

---

## Primitive Kernel Library

Hand-written, SIMD-optimized kernels for the most frequent fused operation patterns.
The ground truth for performance.

See [BACKEND.md §6](./BACKEND.md) for full design.

---

## QCheck Harness

Generates well-typed Core IR programs and checks that treewalk and compiler agree on
all outputs. The correctness guarantee for every backend optimization.

The **Core IR generator** is the first deliverable: a QCheck `Gen` that produces
arbitrary well-typed Core IR programs. This is independent of the parser and can
be built as soon as the treewalk is stable. It provides much broader coverage of
the evaluator than hand-written tests and is a prerequisite for the
treewalk-vs-compiler differential testing that validates every subsequent
optimization pass.

See [BACKEND.md §7](./BACKEND.md) for full design.

---

## REPL

The REPL plugs in after Core Lowering and runs on the treewalk interpreter. Each
expression is parsed and type-checked in the context of the accumulated session
state, lowered to Core IR, and interpreted directly. No fusion, no kernel selection,
no C emission. This gives a correct, low-latency REPL for free once the treewalk
exists. Large array computations will be slow — the performance gap relative to the
compiled backend is expected and acceptable for interactive use.

The REPL requires **incremental type checker state**: a persistent typing environment
that accumulates definitions, hooks, and trait implementations across expressions
without re-checking prior entries from scratch. Each REPL expression extends the
environment; bindings introduced at the prompt are visible to subsequent expressions.
This is the same requirement as an LSP server doing incremental checking on
keystroke, so the incremental state machinery is shared between both.

The incremental type checker state is worth designing carefully from the start:
- Each committed expression produces a **type checker snapshot** — a serialisable
  representation of the environment at that point.
- On error, the snapshot rolls back to the last successful state. The session is
  never left in a partially-committed state.
- Module imports at the REPL prompt extend the snapshot with the imported module's
  public names, resolved hooks, and Z3-cached dimension results.

A later REPL mode can compile expressions through the full pipeline and load the
resulting code natively, closing the performance gap for large array workloads. The
incremental type checker state is reused unchanged for this mode.

---

## Later

- **Loop IR.** A structured loop representation between the fusion passes and C
  emission, expressing SIMD width parametricity, accumulator register conventions,
  predicated execution, tiling, and unrolling as first-class transformations. The
  goal is to generate code that matches or beats the hand-written kernels
  automatically for any fused operation sequence, not just the ones in the initial
  library. The hand-written kernels serve as both ground truth and validation target
  for the loop IR — the QCheck property becomes: for every covered pattern, loop IR
  output matches kernel output on the target hardware.
- **Language server protocol** implementation over the shared frontend, sharing the
  incremental type checker state with the REPL.
- **Compiled REPL mode** — compile expressions through the full pipeline and load
  natively, closing the performance gap for large array workloads.
- **Rank-polymorphic horizontal fusion** and `'scan` horizontal fusion (currently deferred in the spec).

---

## Implementation Order

Build bottom-up: Core IR is the QCheck boundary between the treewalk and compiler
backends, and the type checker's primary job is producing it. Designing Core first
prevents frontend/backend mismatches and gives an early correctness oracle.

- [x] **Core IR definition** — node set, types, invariants. Typed lambda calculus +
  case expressions + explicit effect evidence (Koka-style) + dimensioned array
  operations + primitives. No typeclass dictionaries — hooks are fully resolved
  before Core; no runtime dispatch tables. Dimensions carried as annotations or
  erased to bounds assertions depending on target.
- [x] **Treewalk interpreter** — interprets Core IR directly using OCaml 5 effects.
  Naively correct, zero optimization. Becomes the QCheck oracle for all subsequent
  passes. Also serves as the REPL backend once a parser exists.
- [ ] **Core IR QCheck generator** — a `Gen` for arbitrary well-typed Core IR
  programs. Independent of the parser; can be built now. Exercises the treewalk
  far beyond what hand-written tests can reach and is the prerequisite for
  treewalk-vs-compiler differential testing.
- [ ] **Layout preprocessor** — isolated pass between raw lexer and Menhir grammar;
  inserts virtual `{`, `;`, `}` tokens. Test independently before building the
  grammar on top. Cover the non-trivial cases: `iso` blocks, `'handle` clauses,
  same-line `(` suppression, attribute-scope blocks.
- [ ] **Lexer / Menhir grammar** — surface syntax to untyped `Surface.Ast`. Builds
  on the preprocessor token stream. Take care with juxtaposition call-direction
  (rightmost element is the function) and result-type dispatch hooks (`gen`,
  `from`, `iso` in expression position).
- [ ] **Type checker architecture** — before writing any inference code, produce an
  architecture document covering: incremental snapshot format (shared with REPL
  and LSP), hook specificity decision procedure, fork detection rewrite interface,
  and re-entrant TC interface for Program T. Runs in parallel with the parser
  phases above; must be complete before TC implementation begins.
- [ ] **Core lowering stubs** — wire simple fragments (lambdas, let, case) end-to-end
  through parser → TC → lowering → treewalk as early as possible to validate the
  pipeline architecture. Expand incrementally to the full hook surface once the
  round-trip is proven.
- [ ] **Type checker** — bidirectional inference, hook resolution (keyword-first /
  kind-first / specificity ordering / import-boundary finalization), Z3 dimension
  checking. Implement against the architecture document from the prior step.
  Incremental snapshot mechanism built in from the start.
- [ ] **Program T expansion** — macro fixed-point over the typed AST. Requires the
  TC to be incrementally re-entrant: splice → TC → splice until no new splices
  remain.
- [ ] **Whole-program passes** — specialization / monomorphization, effect-based
  specialization, Layer 1–3 fusion, derived DAG construction, ANF / closure
  conversion, C emission.
- [ ] **REPL** — treewalk backend + parser + incremental type checker state.
  No C emission required. Correct and low-latency; large arrays will be slow,
  which is acceptable for interactive use.
