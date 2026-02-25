# 2. Type System

## 2.1 Base Types and Array Types

Arrays carry their element type and shape in the type. Shape is expressed using
dimension variables:

- `a` — scalar (or rank-polymorphic variable)
- `a[n]` — rank-1 array of `a` with size `n`
- `a[n;m]` — rank-2 array of `a` with sizes `n` and `m`
- `a[]` — array of `a` with unspecified size (escape hatch for dynamic sizes)

Dimension variables (`n`, `m`) may be statically known (compile-time constants)
or existentially quantified (runtime sizes).

**Range types**: `Range Int` is a first-class stdlib type describing a lazy
integer sequence. It has three fields: `start`, `end`, and `step`. A range
literal does not allocate — it is a pure descriptor that materialises to
`Int[]` only when consumed. Ranges can be stored, passed to functions, and
composed freely before any allocation occurs:

```
r  ← 1..10        // r : Range Int — no allocation yet
r2 ← 0..2..20     // r2 : Range Int {start=0, step=2, end=20}
arr[r]            // materialises r as an index slice
r 'each f         // lazy iteration — no Int[] allocation
```

`iter` operations on `Range Int` (`'map`, `'filter`, `'each`, `'fold`, etc.)
are lazy: they consume the range descriptor directly without producing an
intermediate `Int[]`. Only **consumption sites that require a concrete array**
trigger materialisation: index arguments to array slicing (`arr[r]`), writing to
storage, or passing to a function whose signature demands `Int[]`. The explicit
`range 'to Int[]` call documents an intentional forced materialisation.

The strided form `a..step..b` parses LTR as `(a..step)..b`. The first `..`
produces `Range Int {start=a, end=step, step=1}`; the second `..` promotes
`.end` to `.step` and sets the final `.end`. The two hooks are:

```
op .. : Int, Int       → Range Int   // simple range: step defaults to 1
op .. : Range Int, Int → Range Int   // strided: promotes .end → .step, sets new .end
```

Array operations that accept `Int[]` also accept `Range Int` — materialisation
is implicit at consumption sites. Explicit conversion is available via
`range 'to Int[]` when a concrete array is required.

## 2.2 Sum Types and Discriminated Unions

Arra unifies sum types and unions via **discriminated unions**. There is no
separate "union" concept:

- Structs are product types.
- Discriminated unions (tagged unions) are sum types.
- To create a sum type, define the variants as separate types and form a
  tagged union over them. The tag is always present at runtime; the union is
  always discriminated.

This eliminates the conceptual distinction between "sum" and "union".

**Variant payloads are closed**. Each constructor carries a fixed payload type;
variant payloads cannot be open rows. Row polymorphism applies at the
*container* level, not the variant level: a function parameterised over
`{sym: Symbol | rest}` (with `rest : Type`) accepts a discriminated union type
via `project` dispatch or the transparent `from` coercion (§2.3), without any
special open-variant machinery. For columnar access patterns this is sufficient
— `arr 'map .sym` extracts the `sym` field from an array of union values via a
`project` hook on the union type, regardless of which constructor each
element carries.

## 2.3 Row Types

Row types are a **general** mechanism in Arra. Records have polymorphic row
types, supporting extension and restriction operations. Struct definitions
participate in the row type system.

**Core operations:**

- **Access**: `r.field`
- **Extension**: `{r | field : T}` — add a field; type error if already present
- **Restriction**: `r - field` — remove a field
- **Row polymorphism**: pattern `(r : {field : T | rest})` — match on the
  presence of a field, bind the remainder as `rest`
- **Merge**: `r1 ++ r2` — combine two rows; type error if fields overlap

**Lacks constraint:** `{rest | f : T}` implicitly constrains the row variable
`rest` to **lack** field `f` — i.e., `f ∉ rest`. This constraint is inferred
from the extension expression and propagated by the type checker. No new syntax
is required; the constraint is carried on the row variable throughout inference.
A call site that instantiates `rest` with a row already containing `f` is a
static type error. This is consistent with `++` (which also enforces disjoint
field sets statically) and with the row laws — `(r | f : T) - f = r`
presupposes `f ∉ r`.

**Canonical field ordering:**

Row types use **canonical lexicographic ordering on field labels**. Rows are
unordered sets at the type level — `{x: Int, y: Float}` and `{y: Float, x: Int}`
are the same type — but are always stored in lexicographic field order in memory.
Any row expression is normalized to this canonical form by the type checker.

This gives both properties simultaneously: merge is exactly commutative (two
sorted sequences merge deterministically regardless of input order), and memory
layout is fully determined (always alphabetical by field label).

For external layout compatibility (C FFI, binary protocols), a `/'-FieldOrder-'/`
attribute on a specific type declaration overrides the canonical ordering for
that type only.

**Row type laws:**

The laws are organized by the operations they govern. All are derivable from
first principles given canonical lexicographic field ordering.

*Group 1 — Extension, restriction, and access:*

- `(r | f : T) - f = r`
  Extending then restricting is identity. (Also derivable from Groups 2 and 4.)
- `{r | f : T}.f : T`
  Accessing the just-extended field yields a value of its declared type.
- `{r | f : T}.g = r.g` when `f ≠ g`
  Extension does not affect access to other fields.
- `(r - f).g = r.g` when `f ≠ g`
  Restriction does not affect access to other fields.
- `{(r - f) | f : r.f} = r` when `f ∈ r`
  Decomposing a field and reconstructing it is identity.

*Group 2 — Extension as singleton merge:*

- `{r | f : T} = r ++ {f : T}` when `f ∉ r`
  Extension is derived from merge with a singleton row. All extension laws
  follow from merge laws combined with this definition.

*Group 3 — Merge is a commutative monoid:*

- `r ++ {} = r`
  The empty row is the identity for merge.
- `r1 ++ r2 = r2 ++ r1`
  Merge is exactly commutative. Follows from canonical ordering: both sides
  normalize to the same lexicographically sorted field sequence.
- `(r1 ++ r2) ++ r3 = r1 ++ (r2 ++ r3)`
  Merge is associative. Follows from canonical ordering: merging sorted
  sequences is associative regardless of grouping.

*Group 4 — Restriction distributes over merge:*

- `(r1 ++ r2) - f = (r1 - f) ++ r2` when `f ∈ r1, f ∉ r2`
- `(r1 ++ r2) - f = r1 ++ (r2 - f)` when `f ∉ r1, f ∈ r2`

*Group 5 — Access on merge:*

- `(r1 ++ r2).f = r1.f` when `f ∈ r1, f ∉ r2`
- `(r1 ++ r2).f = r2.f` when `f ∉ r1, f ∈ r2`
- `r1 ++ r2` is only well-typed when `fields(r1) ∩ fields(r2) = ∅`
  Field overlap at a merge site is a type error.

*Group 6 — Row polymorphism:*

If a row `r` matches the pattern `{f : T | rest}`, then:
- `r - f = rest`
- `{rest | f : T} = r`

These connect the pattern-matching form to the algebraic operations: the
pattern decomposes `r` into its `f` field and the remainder, and the
original row is reconstructible from those two parts.

*Group 7 — Columnar array laws:*

- `(r[n]).f = r.f[n]`
  Projecting field `f` from an array of records yields the column array for
  that field. Field access and array indexing commute.
- `r1[n] ++ r2[n] = (r1 ++ r2)[n]`
  Merging two same-length arrays of records column-wise is equivalent to an
  array of merged records. This is the fundamental law underlying fact table
  join semantics.

**Arrays of records** have a columnar representation: `{field: a}[n]` maps to
a struct-of-arrays layout in canonical field order. Projecting a field from an
array of records gives an array of that field's values: `({field: a}[n]).field =
a[n]`. This is the fundamental representation for fact table columns.

**Nested record projection**: when `r.a` and `a : {b: T}` (a record-typed
field), the result is the nested record value `{b: T}` — the accessor stops at
one level. Multi-level paths chain: `r.a.b` projects `b` from the nested record.
The columnar law extends to nested paths: `(r[n]).a.b = r.a[n].b = r.a.b[n]`
(field access and array indexing commute at every level).

Row types interact with hook dispatch: a function may be polymorphic over
the fields present in a record, constrained by the row type system. The orphan
rule ([03_HOOKS.md §3.4](03_HOOKS.md#34-orphan-rule)) applies to row-polymorphic hooks.

**Row constraint kinds — `rest : Row` vs. `rest : Type`:**

The `rest` tail variable in `{f: T | rest}` can have one of two kinds,
determining the semantics of the constraint:

- **`rest : Row`** (structural row extension): `{f: T | rest}` is a concrete
  structural row type. `rest` represents the remaining fields and can be
  threaded through to return types. All row laws in this section apply.

- **`rest : Type`** (project-based constraint): `{f: T | rest}` is a
  constraint on the full type `rest`, requiring that `project rest .f → T` is
  defined. `rest` is the entire type being constrained, not a row tail. Any
  type with the appropriate `project` hook satisfies this constraint,
  including nominal `type` definitions.

**Kind inference rule**: if `rest` appears in any type construction or return
position (the right-hand side of `→`, or inside a concrete row expression),
it must be `rest : Row` — structural row construction requires a row-kinded
extension. If `rest` appears only in argument or constraint positions, the
compiler infers `rest : Type` for maximum generality. An explicit `(rest : Row)`
annotation forces structural semantics when the author wants to restrict the
contract.

```
// Row transformation: rest : Row (inferred — rest appears in return position)
addVolume : {sym: Symbol | rest} → {sym: Symbol, volume: Int | rest}

// Field consumption: rest : Type (inferred — rest only in argument position)
chartBySym : {sym: Symbol | rest} → Chart   // accepts Trade or any {sym | ...}
```

**Nominal type definitions and row type compatibility — three forms:**

Record `type` definitions participate in the row type system through three
forms, giving explicit control over the erasing/nominal trade-off:

- **Erasing** (type synonym): purely structural alias, no nominal identity.
  ```
  alias Trade ← {sym: Symbol, price: Float, ts: Timestamp}
  ```

- **Transparent nominal** (default for record `type`): nominal identity plus
  an auto-generated zero-cost `from` coercion to the underlying row type.
  ```
  type Trade ← {sym: Symbol, price: Float, ts: Timestamp}
  // compiler auto-generates:
  // from Trade → {sym: Symbol, price: Float, ts: Timestamp} ← prim.unwrap
  ```
  The `from` coercion fires implicitly in output-dispatch positions ([03_HOOKS.md §3.5.2](03_HOOKS.md#352-output-dispatch-from-gen-and-iso)),
  making `Trade` structurally compatible with `rest : Row` functions. It is
  zero-cost when field layout is canonical (default lexicographic order); if
  `/'-FieldOrder-'/` reorders fields, the coercion requires a field shuffle and
  the compiler emits a note.

- **Opaque nominal** (annotated with `/'-Opaque-'/`): nominal identity only; no `from`
  coercion generated. Use when the type must not coerce structurally.
  ```
  /'-Opaque-'/ type Password ← {hash: Bytes, salt: Bytes}
  ```

The two constraint kinds interact with the three forms as follows:
- `rest : Type` — accepts transparent nominal `Trade` directly via `project`
  hook dispatch; no `from` coercion fired.
- `rest : Row` — accepts transparent nominal `Trade` via the implicit `from`
  coercion; `rest` unifies to the remaining fields of the full row.
- Opaque nominal types satisfy neither form without an explicit `distinct`
  wrapper or user-written `from` hook.

**Columnar array coercion (`flip`):**

For transparent nominal types, the compiler auto-generates the array-level
coercion alongside the scalar one:

```
// auto-generated (scalar):
from Trade → {sym: Symbol, price: Float, ts: Timestamp} ← prim.unwrap
// auto-generated (array):
from Trade[n] → {sym: Symbol[n], price: Float[n], ts: Timestamp[n]} ← prim.unwrap
```

Both are zero-cost under canonical field ordering: `Trade[n]` and `{sym:
Symbol[n], price: Float[n], ts: Timestamp[n]}` share identical struct-of-arrays
layout, so the coercion is a reinterpretation — no data movement occurs. This
is the `flip` isomorphism between row-oriented and columnar representations.
No standalone `flip` function is needed; the `from` hook fires implicitly
in output-dispatch positions like any other `from` coercion.

**Cost model**: zero-copy for canonical field order (default lexicographic).
When `/'-FieldOrder-'/` is used to reorder fields, the canonical struct-of-arrays
layout no longer matches the custom order; the coercion requires an **O(n)**
field shuffle (one pass over the array to permute columns), and the compiler
emits a note at the coercion site identifying the reordered type.

For structural row types (no `type` declaration), `{f: T}[n]` and `{f:
T[n]}` are a layout identity by Group 7: same bytes, no coercion is needed
or generated.

## 2.4 Distinct Types

`distinct` defines a zero-cost wrapper around an existing type, creating a
distinct type with the same runtime representation. Distinct types are required
when defining instances (hooks or trait implementations) for types you do
not own — they are Arra's **newtype wrapper** pattern for implementing external
traits. See [03_HOOKS.md §3.4](03_HOOKS.md#34-orphan-rule).

## 2.5 Dependent Dimension Types

Array dimension types are a form of dependent typing. The dimension variable
syntax already in the grammar (`a[n]`, `a[n;m]`) commits Arra to Pi types: the
type of an array depends on a value.

**What the existing syntax implies:**

- `iota : (n : Nat) → Int[n]` — Pi type; a function from a value to a type
- `concat : a[n] → a[m] → a[n+m]` — return type involves arithmetic on
  dimension variables
- `iter 'rank a[n;m], (1 : Int), (a[] → a) → a[n]` — return type depends
  on a specific value, not just a variable

These three cases represent escalating complexity. Each is addressed differently.

**Sigma types (dependent pairs) for dynamic output sizes:**

Functions like `filter` produce arrays whose size is not statically known.
The principled type is a sigma type bundling the size with the array:

```
filter : a[n] → (a → Bool) → ∃(m : Nat, m ≤ n) a[m]
```

The constraint `m <= n` is a refinement discharged by the SMT backend (see
below). The consumer unpacks the pair. The `a[]` escape hatch remains available
when size tracking is not needed.

**Elimination of sigma types (dependent let-binding):**

Since Arra's sigma types are always dependent products — never sums — the
elimination form is a single-branch match, which is used as a dependent
let-binding. Sigma elimination uses the standard single-branch form — the scrutinee is
applied to a tuple-pattern branch:

```
filter arr pred
  (m, _, filtered) → ...body...
```

What the type checker does at this binding site:

1. Determines the type of the scrutinee: `(m' : Nat, m' <= n, a[m'])` where
   `m'` is the sigma-bound variable, not yet in scope.
2. Binds the first component to a fresh term-level variable `m : Nat`.
3. Discards the proof component (`_`) but **adds its content to the Z3
   context**: `m <= n` is now available to the solver for the entire body scope.
4. Types the array component as `a[m]` via substitution of `m'` with `m`.
5. In the body: `m` is an ordinary value, `filtered : a[m]`, and Z3 knows
   `m <= n` without any explicit annotation.

Z3 handles all arithmetic equality and transitivity automatically. Constraints
accumulate across nested eliminations:

```
filter arr pred1
  (m, _, mid) →
    filter mid pred2
      (k, _, final) →
        // Z3 context: m ≤ n, k ≤ m
        // Z3 derives k ≤ n transitively — no annotation needed
        ...
```

**Runtime representation of sigma types:**

The constraint component is erased at compile time. The runtime representation
of `∃(m : Nat, m ≤ n) a[m]` is simply `(Nat, array)` — a size paired with an
array, which is what any dynamic-size array requires anyway. No overhead beyond
storing the size alongside the data.

**Restriction:** sigma type constraints must lie in the quantifier-free linear
arithmetic fragment. Constraints outside this fragment (e.g., `prime m`) fall
back to `a[]` with runtime shape checks. This is the same boundary as the Z3
backend generally.

**Type-level arithmetic via Z3:**

Size arithmetic over dimension variables is handled by an **SMT backend (Z3)**,
which is load-bearing for core type checking — not merely a library integration.
The supported arithmetic is the quantifier-free theory of linear integer
arithmetic (LIA):

- Linear: `n + m`, `2 * n`, `n - 1` — automatic, always decidable
- Nonlinear: `n * m` (e.g., `reshape : a[n*m] → a[n;m]`) — SMT best-effort;
  falls back to the `a[]` escape hatch with a runtime shape assertion when Z3
  cannot discharge the constraint
- Inequality refinements: `m <= n`, `n > 0` — automatic for linear cases

**`Nat` over Z3 integers**: Z3's LIA fragment is over ℤ, not ℕ. `Nat`
(the kind of dimension variables) is semantically "non-negative integer."
The compiler automatically injects `v ≥ 0` into the Z3 context for every
dimension variable `v` in scope — this is a background axiom, not something
the programmer writes. This preserves LIA decidability; a dedicated `Nat`
sort would lose the decision procedure and is not used. See §2.8 for the
full Z3 integration architecture.

**Value case-splitting (`'rank r` with runtime `r`):**

When `r` is a compile-time constant, the static literal dispatch (§1.3) selects
the correct specialization without a branch table. The following concerns the
runtime fallback when that is not possible.

The `'rank r` iterator requires the return type to depend on the *runtime value*
of `r`: if `r = 1`, the result is `a[n]`; if `r = 2`, the result is `a[m]`.
This requires type-level case-splitting, which goes beyond arithmetic refinements.

`r` must be a compile-time constant. With whole-program compilation and
value-level specialization, this covers the vast majority of practical uses
— rank arguments are almost always literals in real code. When `r` is a
runtime value, the return type falls back to `a[]` with a runtime shape
check. Type-level functions would enable the full dependent type for runtime
`r`; this is deferred, not excluded.

**Escape hatch:**

`a[]` (no dimension annotation) opts out of size tracking entirely. The
dimension is dynamic; no static guarantees are made. Shape errors become runtime
errors. This is the right choice for IO-sourced arrays and other cases where
dimensions genuinely cannot be known statically.

## 2.6 Kind System

Arra has five kinds. A kind classifies what a type-level variable ranges
over:

| Kind | Classifies | Example variables |
|---|---|---|
| `Type` | value types | `a`, `b`, `t` |
| `Nat` | natural number indices (array dimensions and user type parameters) | `n`, `m`, `k` |
| `Effect` | effect rows | `e`, `fx` |
| `Row` | record row tails | `rest`, `r` |
| `Field` | record field names | `f`, `g` |

**Kind inference**: every `∀` binder variable has its kind inferred from
usage. No annotation is required because each structural position in a type
expression admits exactly one kind — the positions are disjoint:

| Type position | Inferred kind |
|---|---|
| `a[n]` dimension slot | `Nat` |
| `T n` where `T` is a known `Nat → Type` constructor | `Nat` |
| `⟪e⟫` effect slot | `Effect` |
| `{... \| rest}` row tail, return/construction position | `Row` |
| `{... \| rest}` row tail, argument-only position | `Type` (see §2.3) |
| field name argument to `assign`, `(.f)` section, etc. | `Field` |
| all other positions | `Type` |

The row tail position is the only one admitting two kinds. Whether `rest` is
`Row` or `Type` is determined by whether `rest` appears anywhere in a **return
or construction position** in the same signature — if so, it must be `Row`; if
only in argument positions, the compiler infers `Type`. All other positions
admit exactly one kind; a variable appearing in conflicting non-row positions is
a kind error.

**"Argument position" defined**: a position is an argument position if it
appears to the left of the outermost `→` in the signature — i.e., in the
domain of the function type, including all parameter slots, nested function
domains, and constraint positions. A position is a return/construction position
if it appears to the right of the outermost `→`, or inside a row literal being
constructed (`{f: T | rest}` on the right-hand side). A row tail that appears
*only* as a function parameter type (e.g., `f : (r : Row) → ...` where `r`
only appears in argument position) is inferred `Type`, not `Row`.

**`Nat`-kinded variables in user-defined type constructors.** `Nat`-kinded
variables are not restricted to array dimension slots. A `type` declaration may
include `Nat`-kinded type parameters, making the declared type a type constructor
of kind `Nat → Type` (or `Nat → Nat → Type`, etc.):

```
type Buffer n ← { data: Byte[n] }         // Buffer : Nat → Type
type Matrix n m ← { data: Float[n; m] }   // Matrix : Nat → Nat → Type
```

Kind inference in `type` bodies uses the same rules as signatures: if a
parameter appears in an `a[n]` slot in the body, it is inferred `Nat`; if it
appears as a plain type, it is inferred `Type`. No annotation is required. A
parameter that appears in both positions is a kind error.

User-defined `Nat`-parameterised types participate fully in the Z3 constraint
system. A function `concat : Buffer n → Buffer m → Buffer (n+m)` generates the
same LIA constraint as `concat : a[n] → a[m] → a[n+m]` — Z3 handles both
identically. The specificity ordering ([03_HOOKS.md §3.2](03_HOOKS.md#32-specificity-ordering)) extends to `Nat` parameters in
user types using the same rules as array dimension slots: `Buffer 3` is more
specific than `Buffer n`, which is more specific than an unconstrained `Buffer`
with no size parameter.

**Example**: in `∀ n a e ⇒ a[n] → ⟪e⟫ a`, the kinds are all inferred:
`n : Nat` (appears in `a[n]`), `a : Type` (appears as a plain type),
`e : Effect` (appears in `⟪e⟫`). No annotation needed.

**Opt-in annotation**: `(n : Nat)` in a `∀` binder is accepted as a
documentation form. It is never required for disambiguation, but is useful
when a variable is used in a non-obvious position or the author wants to
be explicit for a reader. If present, the annotation is checked against
the inferred kind; a mismatch is a compile error.

**Kind errors are reported at the point of inconsistency**, not at the
definition. With whole-program compilation the compiler identifies the
specific use site where two positions assign conflicting kinds to the same
variable.

**Field name literals and the `Field` kind.** Field label literals (`x` in
`{x: Int}`) are **ground constants of kind `Field`** — the same kind as
`Field`-kinded variables, just with fixed identity. This is the exact parallel
to dimension literals: `n` in `a[n]` is a `Nat`-kinded variable; `3` in
`a[3]` is a ground `Nat` constant unified only with itself. Field labels follow
the same pattern: `f` in `∀ (f : Field) ⇒` is a `Field`-kinded variable
(unifiable with any label); `x` in `{x: Int}` is a ground `Field` constant
(`unify(x, y) = ⊥` for `x ≠ y`, `unify(x, x) = {}`). Literals and variables
are the same syntactic category — the distinction is ground vs. unifiable, not
a different kind or a separate parser production.

While `Field`-kinded *variables* range over all possible field names at compile
time, concrete field name literals (`.x`, `.name`) are additionally ground
`Field` *values* at runtime — interned tokens. The stdlib `from Field →
function` hook ([03_HOOKS.md §3.5.2](03_HOOKS.md#352-output-dispatch-from-gen-and-iso)) coerces a concrete `Field` value to its
corresponding accessor function `{x: T | rest} → T` when the type context
demands a function. Field-kinded variables in `∀` binders benefit from the
same coercion at call sites where a function type is expected.

## 2.7 Constrained Polymorphism

Polymorphic types carry constraints expressing which hooks must exist for
the type variables involved. This is constrained polymorphism, unified with
the hook and trait systems described in Section 3.

A first-class hook-dispatched function (e.g., the operator section `(/)`) has a
constrained polymorphic type of the form:

```
∀ a b c ⇒ (a, b) → c
```

where `∀ a b c` asserts that a hook for `/` for `(a, b) → c` exists in
scope. This is resolved at the call site, not at the definition site of `(/)`.
Hook-dispatched operators passed as first-class values carry their constraints with
them and are resolved when the types become known.

Constraints extend to **effect variables**. An effect-polymorphic constrained
type takes the form `∀ a b e ⇒ a → ⟪e⟫ b` where `e` ranges over effect rows.
Effect variables do not create ownership requirements for the orphan rule —
they are not types that any module "owns."

**Ambiguity**: when the concrete types cannot be inferred from context, a
**type annotation is required**. The compiler reports the ambiguity at the point
where inference fails and does not default silently. With whole-program
compilation, the compiler can report which specific call sites are ambiguous
rather than which definitions are underspecified.

## 2.8 Z3 Integration Architecture

Z3 is invoked as a subprocess (or linked library) during type checking.
This section specifies the integration contract.

**Invocation model — batch per definition:**

Z3 is not invoked per constraint. Instead, all constraints generated while
type-checking a single top-level definition are collected and submitted to
Z3 in one call. This allows Z3 to reason over the full constraint context
at once and amortizes the invocation overhead. The constraint set for a
definition includes:

- Background axioms: `v ≥ 0` for every dimension variable `v` in scope
- Constraints from the definition's type signature and hook heads
- Constraints accumulated from sigma type eliminations in the body
- Guard conditions from `when` clauses (linear arithmetic subset only)

**Dimension variable freshness:**

Before a definition's constraint batch is submitted, every dimension variable
is **alpha-renamed** to a globally unique identifier encoding its definition
site (e.g., `n` in `f` becomes `f$n`; `n` in `g` becomes `g$n`). Name
collisions across definitions are structurally impossible — two definitions
sharing the surface name `n` are distinct Z3 variables by construction.

Each batch is checked in an **isolated Z3 context**. The implementation may
use separate solver instances or `push`/`pop` scopes; either is correct.
After the result is returned, the context is discarded or popped — no solver
state leaks between definitions. The alpha-renamed variable identifiers are
also used as keys in the constraint provenance table (§2.8 UNSAT policy),
so error messages can identify the precise definition and binding site of
each conflicting constraint.

**Caching and incremental compilation:**

Z3 results are cached content-addressed on the constraint set. The cache
is keyed on the serialised constraint set; if the same constraints appear
again (e.g., in a recompile where a definition is unchanged), the cached
result is reused without invoking Z3. Invalidation follows the module
boundary: when a module is recompiled, its cached Z3 results are
discarded. Modules that are not recompiled retain their cached results.

**UNSAT policy:**

When Z3 returns `UNSAT`, the constraint set is provably contradictory — this
is a **type error** in the programmer's code, not a solver limitation. The
compiler extracts a **minimal UNSAT core** via Z3's `get_unsat_core` and maps
each assertion in the core back to its source location using a constraint
provenance table maintained during type-checking. The error message names each
conflicting constraint with its origin:

```
type error: contradictory size constraints in `reshape2d`
  (1)  m ≤ n        — from sigma elimination at reshape2d.arra:14:8
  (2)  m > n        — from when-guard at reshape2d.arra:21:3
  constraints (1) and (2) cannot both hold
```

Each assertion submitted to Z3 carries an internal label keyed to its source
span. The provenance table is built incrementally as constraints are generated
during type inference; it is discarded after Z3 returns (not persisted to the
cache).

**Timeout policy:**

Z3 has a configurable per-definition timeout set at project level (default
value TBD; expected order of magnitude: hundreds of milliseconds). A timeout
is a **hard error**: the compiler reports which constraint could not be
discharged within the budget and marks the definition as having a type error.
Compilation ultimately fails.

**Continued checking after timeout.** The compiler does not stop checking the
rest of the definition or the rest of the module. At the timed-out site, the
sigma type is internally degraded to `a[]` for the purposes of continuing the
type-check pass. This lets the compiler report all remaining errors in the
module in a single pass — the programmer sees the timeout error and any
independent downstream errors together. The degraded `a[]` is not written to
the module's exported interface; callers see a type error on the definition,
not a silently weakened signature.

This degradation is explicitly diagnostic — it always accompanies a hard error
at the timeout site — and does not make `a[]` mean two things. An explicit
`a[]` annotation in source is a programmer choice; a degraded `a[]` from a
timeout is a compiler diagnostic artifact, always paired with an error.

**Per-definition budget override:**

```
/'-Z3Budget 5000 -'/    // override to 5000 steps for this definition
f : a[n] → a[n*2]
f ← ...
```

The budget is measured in Z3 **solver steps** (not wall time) for
reproducibility across machines with different CPU speeds. The project-level
default is overridden at the definition site. This is the escape hatch for
constraints that are decidable but expensive — the programmer acknowledges
the cost explicitly.

**Z3 version pinning**: solver step counts are not stable across Z3 versions
— the same constraint may take a different number of steps in Z3 4.12 vs.
Z3 4.13. The project's Z3 version is therefore a build dependency, specified
in the project manifest. Upgrading Z3 may require re-tuning `/'-Z3Budget N-'/`
annotations.

**Programmer escapes for persistent timeouts:**

1. Simplify the constraint (refactor or split the definition).
2. Raise the budget with `/'-Z3Budget N-'/`.
3. Explicitly annotate the dimension as `a[]` — opts out of static
   verification for that dimension and inserts a runtime shape assertion.

**Constraint composition across function boundaries:**

The assume/guarantee model applies at **every function boundary**, within a
module and across modules alike. There is no special case for within-module
calls. Each function's signature is verified once against its body; callers
use the signature as a trusted discharged lemma without looking into the body.

Each module verifies its own definitions against their exported type
signatures. Verified signatures become **trusted assumptions** for all
callers — Z3 does not re-verify a called function; it uses the exported
signature as a discharged lemma.

At link time (whole-program compilation), constraints crossing module
boundaries are already decomposed into: (A) verified signatures from
dependency modules (trusted), and (B) call-site constraints in the
current module (checked locally). No cross-module Z3 query is required.
The incremental model handles invalidation: if a dependency module's
signature changes, the dependent module is recompiled and its Z3
constraints are rechecked against the new signatures.

**Sigma constraints from cross-module eliminations:** when a caller
eliminates a sigma type returned by a dependency — e.g., pattern-binding
`(m, _, arr) ←` a value of type `∃(m : Nat, m ≤ n) a[m]` — the proof
component is discarded but its content (`m ≤ n`) is added to the caller's
per-definition Z3 batch as an explicit hypothesis at the binding site
(§2.5). This is the same mechanism as local elimination; no separate
cross-module constraint propagation is required. The constraint is available
throughout the remainder of the definition's body scope.
