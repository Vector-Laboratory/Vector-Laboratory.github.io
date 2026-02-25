# 6. Reactive State and Derived Views

## 6.1 Event-Sourced State

Mutable program state is held in **`⟪State s⟫` cells** — values of type `s`
that can be read, written, and modified via the `⟪State⟫` effect. The
recommended pattern for persistent, queryable state is **event-sourced**:

- An **event log**: an append-only sequence of events; the canonical source
  of truth.
- A **state cell**: the current state, computed by folding the event handler
  over all past events.
- An **event handler**: a function `Event → ⟪State DB⟫ ()` that updates
  the state cell atomically in response to each new event.

```
data DB ← {people: HashMap PersonId Person, employments: Employment[]}

data Event ←
  | Hired  Employment
  | Left   PersonId

processEvent : Event → ⟪State DB⟫ ()
processEvent ←
  Hired emp → modifyState (db → db.(employments ← db.employments 'append emp))
  Left  pid → modifyState (db → db.(employments ←
                             db.employments 'filter (e → e.employee /= pid)))
```

The event log is the source of truth. The current state is always
reconstructible: `events 'fold processEvent emptyDB`. Time-travel queries
follow for free: fold only the first `n` events to recover state at version
`n`. The whole-program compiler fuses the fold with any subsequent queries,
eliminating intermediate states.

## 6.2 `derived` Declarations

A `derived` declaration defines an **incrementally-maintained view** — a named
value the compiler keeps consistent with its source `⟪State⟫` cell(s) as they
change. See [SYNTAX.md §9.9](../SYNTAX.md#99-derived-view-definitions) for the surface syntax.

```
derived byEmployer   ← db.employments 'groupby .employer
derived activeTrades ← db.trades 'filter (t → t.status = Active)
```

The compiler guarantees: after every event handler completes, all `derived`
values are **consistent** — equal to the result of a full recompute from the
current state. Updates are applied atomically with the state transition.

**Derived views may be chained.** A `derived` expression may reference other
`derived` values. The compiler propagates deltas through the chain, applying
each level's delta function in sequence.

### 6.2.1 The `Delta` Type

The compiler represents collection changes using the stdlib `Delta` type:

```
type Delta a ←
  | Insert a
  | Delete a
  | Update {old: a, new: a}
```

`Update` is a primitive, not sugar for `Delete` + `Insert`. A delta handler
may exploit element identity in the `Update` case — for example, a BST can
update a node in-place. For event-sourced state (§6.1), `Update` arises when
an event replaces a row rather than creating or deleting one.

### 6.2.2 Incrementalization Levels

The compiler incrementalizes `derived` expressions using three levels,
determined by annotations on the `iter` hooks involved.

**Level 1 — Built-in SOACs (automatic, no annotation)**

The following operations are maintained in O(1) per element change, with no
annotation required:

| Operation | Delta strategy |
|---|---|
| `'map f` | Apply `f` to changed element only |
| `'filter p` | Re-evaluate `p` on the changed element |
| `'groupby` | Insert/remove/update the affected bucket |
| equi-`'join` | Hash-probe the opposite side on each delta |
| `'count` | Increment / decrement |
| `'sum` | Add / subtract element value |
| `'distinct` | Reference-count per value |

**Level 2 — `/'-CMonoid -'/` annotation (algebraic aggregations)**

A custom `iter` hook may be annotated with `/'-CMonoid -'/`, signalling
that the compiler should derive the delta function from the `CMonoid` trait:

```
trait CMonoid a r
  bop + : r, a → r       // combine: insert element into accumulator
  bop - : r, a → r       // remove: undo element's contribution
  from  : () → r         // zero element
```

The derived delta:

```
delta acc (Insert x)          = acc + x
delta acc (Delete x)          = acc - x
delta acc (Update {old; new}) = (acc - old) + new
```

Example — making `'max` O(log n) via a heap-backed `CMonoid`:

```
// stdlib: MaxHeap a accumulates elements; heapTop extracts the maximum
implementation CMonoid (MaxHeap a) a
  op + ← heap_insert
  op - ← heap_remove
  from ← () → emptyHeap

iter /'-CMonoid -'/
'max : (a : Ord)[n], () → a ← heap → heap heapTop
```

The `CMonoid` accumulator type may differ from the user-visible result type:
`MaxHeap a` is the internal accumulator; the user-visible result is `a` via
`heapTop`. The compiler inserts the projection transparently.

In debug builds, the compiler verifies the identity law (`(acc + x) - x = acc`)
and commutativity (`(acc + x) + y = (acc + y) + x`) as property-based tests
at the definition site using the `Testable` protocol ([05_EFFECTS.md §5.3](05_EFFECTS.md#53-effect-polymorphic-array-operations)) to generate sample
values. A failing test is a **compile error**; if the accumulator type does not
implement `Testable`, a warning is emitted and the test is skipped.

**Level 3 — `/'-Delta f -'/` annotation (full expressivity)**

The programmer provides an explicit delta function. This handles non-algebraic
structures — sorted indexes, topological sorts, running medians, graph
reachability indexes — that Level 2 cannot express:

```
iter /'-Delta bstDelta -'/
'sort : (a : Ord)[n], () → (a : Ord)[] ← prim.sort

bstDelta : (a : Ord)[], Delta (a : Ord) → (a : Ord)[]
bstDelta ←
  tree (Insert x)              → tree 'bstInsert x
  tree (Delete x)              → tree 'bstDelete x
  tree (Update {old; new})     → tree 'bstDelete old 'bstInsert new
```

The delta function takes the current derived value and a `Delta`, and returns
the updated value. It must be **pure** (no effect row) and may use arbitrary
pure logic, including stateful traversal, data structure rebalancing, or
selective fallback to full recompute.

**Level 4 — No annotation (full recompute)**

If an `iter` hook in a `derived` expression carries no incrementalization
annotation, the compiler falls back to recomputing the derived value in full
after each relevant state change. A **note** (not a warning, not an error) is
emitted identifying the operation that forced the fallback. No correctness is
lost; only efficiency.

### 6.2.3 Debug and Release Consistency

For Level 3 delta functions, the compiler automatically inserts consistency
assertions in debug builds:

```
// After each delta application (debug mode only):
assert derived_val = base_collection 'baseOp     // full recompute check
```

This is O(n) per change in debug, O(1) in release. Any divergence between the
incremental result and the full recompute is reported as a runtime error naming
the delta function and the triggering input. The assertion ensures that
incorrect delta functions are caught during development rather than silently
diverging in production.

The assertion may be suppressed with `/'-SkipDeltaCheck -'/` on the delta
function definition — an explicit acknowledgement that the full recompute is
either impractical or the incremental algorithm is the definition of
correctness. Level 1 and Level 2 operations do not require assertions; their
delta logic is correct by construction or verified via CMonoid laws.

### 6.2.4 Multiplicity Assumption

`derived` maintenance assumes each logical element appears in the source
collection at most once — no multiplicity tracking. In the event-sourced
state model (§6.1), this is satisfied structurally: each event creates or
deletes a row with a unique identity. For collections with genuine duplicates,
the programmer encodes reference counts in the accumulated state and handles
them explicitly in a Level 3 delta function.

### 6.2.5 Chained Views and Diamond Dependencies

A `derived` view's delta type is `Delta T` where `T` is the view's value
type — the same stdlib `Delta` type used at the base level. The compiler
derives the appropriate `Delta T` from `Delta S` (the source change) using
the Level 1/2/3 logic for the operation involved. No separate delta type is
needed for intermediate views.

**Update ordering — topological:** after every event handler, the compiler
updates all `derived` views in **topological order** of the dependency DAG.
A view is not updated until all of its own `derived` dependencies have been
updated for the current event.

**Diamond dependencies:** when two views `a` and `b` both derive from a
common source, and a view `c` depends on both, the update sequence is:

1. Update `a` from the source delta.
2. Update `b` from the source delta.
3. Update `c` from the `a` delta.
4. Update `c` from the `b` delta.

Each Level 3 delta function call processes one `Delta` at a time. In the
diamond case, `c`'s delta function is called twice — once for each upstream
change — in DAG-topological order. No intermediate state of `c` is
externally observable between these calls.

The entire update pass is an **internal compiler transaction**: no query
function or external caller can observe any `derived` value until all
updates for the current event are complete.

**Atomicity enforcement — pure delta functions:**

Delta functions are required to be **pure**: their signatures carry no effect
row. `bstDelta : (a : Ord)[], Delta (a : Ord) → (a : Ord)[]` is the canonical
form — no `⟪e⟫` annotation. The compiler rejects any delta function that
carries `⟪IO⟫` or any other effect.

This is the mechanism that makes the atomicity guarantee a compile-time
property rather than a runtime one. Since the update pass is pure and Arra
makes IO explicit via `⟪IO⟫`, the type system ensures that no IO operation
can occur during the pass. IO callbacks are the only way for external code
to observe state; because none can fire during a pure computation, no
intermediate derived state is ever externally visible. No mutex, double-buffer,
or snapshot-swap is needed.

If an update step needs side effects — logging, metrics, diagnostics — the
appropriate mechanism is the `TabledLogEntry` hook (§6.4) or an analogous
hook that is invoked by the runtime *after* the full update pass completes,
not inline within the delta function.

**Level 4 fallback in chains:** when a view at some depth forces Level 4
(full recompute), the compiler performs the recompute for that view and then
propagates the resulting deltas downstream as normal. Downstream views are
not forced to Level 4 by their upstream falling back.

**Delta failure handling:** the event log is committed first; derived state
updates follow. If a delta function signals failure mid-transaction (e.g., an
unrecoverable arithmetic error or a violated invariant):

1. The event is marked **failed** in the log — it is recorded but flagged so
   that future replays can skip or special-case it.
2. The partial derived update is **discarded**. The runtime triggers a Level 4
   full recompute from the event log (skipping failed events) to restore all
   derived views to a consistent state.
3. The failure is reported to the event handler's caller as a `⟪Fail DeltaError⟫`
   effect, where `DeltaError` is a stdlib type carrying the delta function name,
   the view name, and the triggering `Delta` value. The caller handles it via the
   standard handler mechanism ([05_EFFECTS.md §5.1.2](05_EFFECTS.md#512-effect-handlers)), yielding a `Result DeltaError ()` or
   propagating the failure further.

Delta failures are programming bugs, not expected control flow. A delta
function that can legitimately produce no update should return a no-op `Delta`
rather than failing. The Level 4 recompute cost is acceptable for what should
be an exceptional case.

## 6.3 Query Predicates

Queries over base or derived state are `⟪Fail⟫` and `⟪Nondet⟫` predicates —
ordinary Arra functions. No special query syntax is provided.

```
// Point lookup (⟪Fail⟫ — zero or one result)
lookupPerson : PersonId → ⟪Fail⟫ Person
lookupPerson ← pid → db.people 'find pid

// Scan with filter (⟪Nondet⟫ — zero or more results)
activeAt : OrgId → ⟪Nondet⟫ Employment
activeAt ← orgId →
  byEmployer 'find orgId     // ⟪Fail⟫: lookup group, or fail if no such org
  'generate                  // ⟪Nondet⟫: yield each employment in group

// Equi-join: join employments to people
employedPeople : OrgId → ⟪Nondet⟫ Person
employedPeople ← orgId →
  emp ← activeAt orgId
  db.people 'find emp.employee
```

Mode checking applies to all query predicates: each predicate may have
multiple modes specifying which arguments are inputs and outputs.
The determinism of each mode is its effect type (`⟪Fail⟫` for semidet,
`⟪Nondet⟫` for nondeterministic). The compiler verifies that all declared
modes are correctly implemented and that call sites are consistent with the
mode of the predicate being called (see [05_EFFECTS.md §5.4](05_EFFECTS.md#54-nondeterminism-as-an-effect)).

**Multi-mode predicates are multiple hooks.** Because all Arra arguments
are values (there are no unbound logical variables), different modes of a
predicate always correspond to distinct type signatures — differing in
argument count, argument types, or effect type. Mode selection at a call site
is therefore ordinary hook dispatch (§3.1–3.3); no separate mode-inference
pass is required. The type signatures are the mode annotations.

**Call-site ambiguity** between two mode hooks is ordinary hook
ambiguity — resolved or rejected by the specificity ordering in §3.2. No
special case is needed.

**Semantic coherence** — the requirement that all modes of a predicate
implement the same abstract relation — is a **programmer obligation**, not
compiler-enforced. The type system verifies each mode hook independently;
proving that two hooks agree on the same mathematical relation is
undecidable in general. This is the same position as `Distributable` law
verification ([05_EFFECTS.md §5.3](05_EFFECTS.md#53-effect-polymorphic-array-operations)): the contract is documented, not mechanically checked.

> **Future**: a `/'-Modes-'/` attribute could group named hooks as modes
> of the same relation, enabling the compiler to generate property-based test
> scaffolding in debug builds (analogous to `Distributable`) that exercises
> multiple modes on the same data and checks consistency. Deferred pending
> evidence of need.

## 6.4 Recursive Inference with `/'-Tabled -'/`

For derived facts requiring recursive computation — transitive closure,
reachability, type inference, rule-based deduction — the `/'-Tabled -'/`
attribute on a recursive predicate enables fixpoint semantics:

```
/'-Tabled -'/
rec manages : PersonId → PersonId → ⟪Nondet⟫ ()
manages ← mgr report →
  isDirectReport mgr report
  | mid ← isDirectReport mgr 'generate
    manages mid report
```

A tabled predicate uses **top-down evaluation with tabling**: results are
cached per argument tuple; recursive calls with in-progress arguments return
cached results so far; when new results are added the suspended calls are
retried; fixpoint is reached when no new results are produced. This
correctly handles left-recursive rules that would loop in plain Prolog.

Tabled predicates are **ephemeral and query-time**: results are not persisted
and are recomputed on each query invocation. They are not `derived` values.
The runtime implementation is provided by the `Tabled` stdlib module using
the existing `⟪Nondet⟫` coroutine machinery; no compiler extension is required.

**Termination contract**: a tabled predicate terminates iff the set of facts
reachable from the query is finite. The programmer is responsible for this
property. For potentially large result sets, use the standard `⟪Nondet⟫`
consumer operators — `'take`, `'first`, `'find` — to bound evaluation at the
call site; these short-circuit the fixpoint computation as soon as enough
results have been produced.

**Step-budget logging**: evaluation is never aborted by the runtime, but a
configurable step-count threshold triggers a **log hook** when crossed.
This is observability, not interruption — evaluation continues normally after
the hook fires. The hook receives a structured log entry:

```
type TabledLogEntry ← {predicate: Symbol, steps: Int}

// Project-level hook (configured in project settings):
tabledLog : TabledLogEntry → ⟪IO⟫ ()
```

The default hook writes to stderr. Any `⟪IO⟫` implementation may be
substituted — write to a log file, push to an external store, emit a metric.
The step threshold is set project-wide (default: no logging) and may be
overridden per predicate via the attribute argument:

```
/'-Tabled budget:5000 -'/
rec manages : PersonId → PersonId → ⟪Nondet⟫ ()
manages ← mgr report → ...
```

To materialize a recursive derived fact as a `derived` view (persistent,
incrementally maintained), define it using `derived` with explicit Level 3
delta logic — the delta function handles the fixpoint incrementally.

## 6.5 Exported `derived` Values

A `derived` value may appear in a module `interface` block, making it
externally readable as a **snapshot**:

```
module Trades
  interface
    derived activeTrades : Trade[]
    derived bySymbol     : HashMap Symbol Trade[]
  implementation
    derived activeTrades ← db.trades 'filter (t → t.status = Active)
    derived bySymbol     ← db.trades 'groupby .sym
```

The interface entry gives the name and type only; the `← expression` lives
in the implementation block. External callers read the current value as an
ordinary read:

```
open Trades
n ← activeTrades 'count    // reads snapshot; always consistent
```

**Read-only:** external callers cannot write to an exported `derived` value.

**Snapshot semantics:** a caller always sees the state after the most
recently completed event handler — never a partially-updated intermediate.
The consistency guarantee (§6.2) applies at the module boundary.

**No subscription:** exported `derived` values do not support push
subscriptions. Callers read on demand; the module's event handler drives
updates. A reactive stream layer is possible in user space using `⟪Nondet⟫`
but is not part of `derived` semantics.

## 6.6 State Persistence

`derived` state is **ephemeral**: on process restart, all derived views are
rebuilt by replaying the event log from the beginning. This is always correct
— the event log is the source of truth — but may be slow for long-lived
processes with large logs.

**Checkpointing** is a runtime stdlib optimization. A checkpoint serializes
the current state cell together with a position marker into the event log.
On startup, the runtime may load a checkpoint and replay only the events
after the checkpoint position, skipping the earlier replay.

The compiler auto-generates serialization and deserialization for any `type`
definition used as a state cell. No programmer annotation is needed.

**Type compatibility:** every checkpoint embeds a **type hash** of the state
type at write time. Loading a checkpoint whose type hash does not match the
current program's state type is a **runtime error**; the process falls back
to full log replay automatically. This prevents silently loading a stale
checkpoint against a changed schema.

**Schema migration:** when the state type changes, the programmer writes an
explicit migration function and registers it with the runtime:

```
migrate : OldDB → NewDB
```

The runtime applies it when loading a checkpoint whose hash matches `OldDB`.
The compiler does not attempt to derive migrations automatically.
