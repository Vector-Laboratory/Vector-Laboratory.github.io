# 3. Hook System

## 3.1 Dispatch Keywords

Every hook keyword names a **syntactic position** — a point in the surface
syntax where type-directed dispatch fires. The keywords are organized by their
**optic category** and **dispatch direction**:

| Keyword | Optic | Direction | Dispatches on | Syntactic trigger |
|---------|-------|-----------|---------------|-------------------|
| `uop` | — | input | operand type | postfix: `val sym` |
| `op` | — | input | left × right types | infix, value arg: `val sym val` |
| `iter` | Traversal (fold) | input | container type (left) | infix, fn arg: `val sym fn` |
| `app` | Prism (review) | input | callable type (left) | call: `callable arg` |
| `pat` | Prism (preview) | input | scrutinee type | pattern: `Ctor(x) →` |
| `project` | Lens (get) | input | container type (left) | field/index read: `r.x`, user types |
| `assign` | Lens (set) | input | container type (left) | field/index write: `r.(x←v)` |
| `view` | Getter | input | container type (left) | field access: `r.x`; computed, no setter |
| `gen` | Traversal (unfold) | **output** | result container type | infix, fn arg: `seed sym fn` |
| `from` | Iso (one dir.) | **output** | target type (result) | coercion / annotation context |
| `iso` | Iso (both dirs.) | **output** | target type (result) | coercion pair + round-trip laws |

**Dual pairs**: `app` ↔ `pat` (prism: construct / deconstruct); `project` ↔
`assign` (lens: get / set); `iter` ↔ `gen` (traversal: fold / unfold).
`from` declares one direction of an isomorphism; `iso` is the paired form —
it declares both directions simultaneously and asserts round-trip laws (§3.5.2).
`view` is unpaired — a Getter with no corresponding setter. Data constructors
are built-in `app`/`pat` pairs. Structural record field access is a
compiler-handled AST node; `project`, `assign`, and `view` provide the
dispatch hooks for user-defined and non-record types.

**Input vs. output dispatch**: all keywords except `gen`, `from`, and `iso`
dispatch on the type of the left-hand argument at the call site. `gen`,
`from`, and `iso` are **output-polymorphic** — the compiler selects the
hook based on the result type, determined by a type annotation or downstream
inference. When the result type cannot be inferred, a type annotation is
required; the compiler reports the ambiguity at the use site.

**`vec` and `dict` are retired**: they were construction hooks for sequence and
key-value literal notation, now subsumed into the built-in `Sequence` and
`Mapping` traits (§3.5). The literal `[a; b; c]` triggers `Sequence.from`;
`{k: v}` triggers `Mapping.from`. The correct implementation (Array,
List, HashMap, BTreeMap, …) is selected by type annotation or inference.

**Disambiguation algorithm** (left-to-right, greedy):

1. **Keyword-first**: determine which keyword pool applies. `uop` is
   syntactically distinguished by having no right argument — the operator follows
   its operand with nothing after it. For symbolic forms with a right argument,
   the right argument determines the pool via the following procedure,
   in order:
   - **Lambda expression** (syntactically a function literal) → `iter` / `gen`.
   - **Named reference** → look up its type from its definition signature (all
     definitions carry types in whole-program compilation; no inference pass
     required). If function type → `iter` / `gen`; if value type → `op`.
   - **Genuinely ambiguous** (unconstrained type variable with no structural
     information) → annotation required; the compiler reports an error at the
     call site. In a well-typed program this case does not arise: a reference
     that is ambiguous here is already a type error for other reasons.
   `project` and `assign` are distinguished by their field/index argument form.
   `app` fires in call position; `pat` fires in pattern position; `from` /
   `gen` fire when the result type drives resolution. See SYNTAX.md §6 for
   parsing rules, including the two-consecutive-lower rule that resolves the
   `tick lower lower` ambiguity.
2. **Specificity ordering**: within the selected pool, the most specific
   matching hook wins (§3.2).
3. **Error on ambiguity**: if no single most-specific hook exists, it is a
   compile error at the call site.

Hooks that would be ambiguous are rejected at the **definition site** (§3.3).
For any given `(left_type, arg_type)` pair (or result type, for output-dispatch
keywords), at most one hook may exist after the keyword-first pass.

## 3.2 Specificity Ordering

Hook resolution uses a **partial order on type patterns**, following Julia's
method specificity. More specific patterns take priority. The order is:

```
literal value pattern         (1 : Int)      // most specific
concrete type + literal dim   Int[3]
concrete type + dim variable  Int[n]
concrete type + dyn dim       Int[]
concrete type, no dim         Int
trait-constrained + dim       (a : Num)[n]
trait-constrained + dyn dim   (a : Num)[]
trait-constrained             (a : Num)
unconstrained + dim           a[n]
unconstrained + dyn dim       a[]
unconstrained                 a              // least specific
```

- **Function types** decompose over domain and codomain, mirroring the array
  dimension lattice:

  ```
  Int → Bool   both concrete       — concrete type tier (same as Int, Bool)
  Int → b      concrete domain, variable codomain
  a → Bool     variable domain, concrete codomain
  a → b        both variable       — more specific than bare a
  a            unconstrained       — least specific
  ```

  Four rules follow directly:

  1. **`a → b` > `a`**: a function-shaped pattern carries structural
     information (the value must be a function); bare `a` carries none.
     Same logic as `a[n]` > `a`.
  2. **`a → b` is incomparable with `(a : Trait)`**: one constrains the
     shape (must be a function); the other constrains the behaviour (must
     implement a trait). Neither is a subset of the other.
  3. **Cross-position sharing applies**: `(a → a)` (endomorphism) is more
     specific than `(a → b)` (any function) — same rule as `(a[n], a[n])`
     > `(a[n], b[m])`.
  4. **Concrete function types are disjoint**: `Int → Bool` and
     `String → Int` are structurally disjoint; they cannot conflict.

- Array rank (number of dimensions) is **not** a subtype relationship: `a[n]`
  and `a[n;m]` are incomparable. Ambiguity between them is impossible by type
  structure.
- **`Nat` parameters in user types** follow the same specificity rules as array
  dimension slots. For a user type constructor `T : Nat → Type`, the ordering
  is: `T 3` (literal) > `T n` (variable) > `T` (no parameter). Cross-position
  variable sharing applies equally: `(Buffer n, Buffer n)` is more specific than
  `(Buffer n, Buffer m)`.
- **Cross-position variable sharing is a specificity tiebreaker.** A signature
  where the same type variable appears in multiple positions is strictly more
  specific than the same signature with those positions replaced by distinct fresh
  variables. The shared variable imposes an implicit cross-position equality
  constraint, narrowing the match set to a proper subset. Example:
  `(a[n], a[n])` is more specific than `(a[n], b[m])` — the former matches only
  same-type, same-dimension pairs; the latter matches any two arrays. They are
  **comparable** (not incomparable): `(a[n], a[n])` wins when both could apply.
  The §3.3 ambiguity check therefore never fires for this pair; no disambiguating
  hook is required.
  This tiebreaker applies uniformly across **all** positions in a signature —
  including source→result sharing for output-dispatch keywords (`from`,
  `gen`). For example, `from a[n] → a[n]` is more specific than
  `from a[n] → b[m]`: the shared `a` and `n` constrain source and result to the
  same element type and dimension, narrowing the match set exactly as a shared
  variable between two argument positions would.

- **Joint specificity for `from` and `gen`**: these keywords dispatch on the
  *result* type, so the result type is the **primary** specificity dimension —
  only hooks whose result type pattern matches the demanded type are
  candidates. Among candidates with equally-specific result types, the **source
  type** (for `from`) or **seed type** (for `gen`) is the secondary dimension,
  ranked by the same table. Hooks that differ only in source type with the
  same result type must be comparable; an ambiguous pair is a definition-site
  error (§3.3).
- When two hooks are **incomparable** in specificity and both could match a
  given call, this is a compile error **at the definition site of the second
  hook**. The programmer must define an explicit hook that is more
  specific than both.

## 3.3 Definition-Site Ambiguity Check

When a new hook `O_new` is added to operator `sym`, it is checked pairwise
against every existing hook `O_i` for the same `sym` and kind (operator or
modifier, as determined by the kind-first pass in §3.1). The check has two
steps:

**Step 1 — Incomparability.** Determine whether `O_new` and `O_i` are
incomparable under the specificity order (§3.2): neither is more specific than
the other. If one is strictly more specific, no conflict exists — the more
specific hook always wins when both could match.

**Step 2 — Pattern intersection.** For incomparable pairs, determine whether
their `(left_type, arg_type)` pattern signatures have a non-empty intersection:
is there a concrete call that could match both? Decided by **pattern
unification** applied component-wise to the full signature:

```
// Concrete types are nominal; disjoint unless equal
unify(T, T)               = {}          // success
unify(T1, T2)             = ⊥           // fail: T1 ≠ T2

// Type variable instantiates under constraint propagation
unify(a,      T)          = { a = T }
unify(a : C,  T)          = { a = T }   if known T : C, else ⊥

// Two constrained variables: requires a shared witness
unify(a : C1, b : C2)     = { a = b }   if ∃ τ s.t. τ : C1 ∧ τ : C2
                          = ⊥           otherwise

// Same variable, two constraints: compatible if any type satisfies both
unify(a : C,  a : C')     = {}          if ∃ τ s.t. τ : C ∧ τ : C'
                          = ⊥           otherwise

// Dimensions: LIA satisfiability
unify(T[k],   T[n])       = { n = k }   // n is a variable, k a literal
unify(T[k1],  T[k2])      = ⊥           // two distinct literals

// Dynamic dimension: a[] is always comparable to a[n] by §3.2 (a[n] is more
// specific), so unification is never invoked for dispatch. Rule listed only
// to keep unify() a total function over all valid type pairs.
unify(T[],    T[n])       = {}

// Rank mismatch: structurally disjoint (also incomparable by §3.2)
unify(a[n],   a[n;m])     = ⊥

// Closed records: disjoint if field sets differ
unify({f₁: T₁…fₙ: Tₙ}, {g₁: U₁…gₘ: Uₘ})
                          = unify(T₁, U₁) ∪ … ∪ unify(Tₙ, Uₙ)
                                           if field sets equal (same names, same order)
                          = ⊥             otherwise
// Open-record patterns ({… | r}) are comparable by §3.2 (more fields = more
// specific), so they are never incomparable and unification is never invoked.

// Tuple arity: different arities are structurally disjoint
unify((a, b), (a, b, c))  = ⊥           // and generally: arity mismatch → ⊥

// Full signatures: both components must succeed (conjunction)
unify((L1, R1), (L2, R2)) = unify(L1, L2) ∪ unify(R1, R2)
```

The shared-witness check `∃ τ s.t. τ : C1 ∧ τ : C2` is decidable at compile
time because Arra uses whole-program compilation: all trait implementations are
known when each hook is checked.

**Error and fix.** If both steps succeed, `O_new` is rejected at its definition
site. The error names both conflicting hooks and a witness type (from the
unification result) that would produce an ambiguous call. The fix is always to
add an explicit hook more specific than both:

```
// Conflict: (a : Num) and (a : Eq) are incomparable;
// any type implementing both (e.g. Int) produces an ambiguous call
op + (a : Num), (a : Num) → a  ← ...   // existing
op + (a : Eq),  (a : Eq)  → a  ← ...   // ERROR: witness Int : Num ∧ Int : Eq

// Fix: explicit hook for the intersection; total order restored
op + Int, Int → Int ← ...
```

**Non-conflict: structurally disjoint patterns.** Incomparable hooks with
provably empty intersection require no disambiguator:

```
// Int[n] and Float[n] are incomparable (same specificity level)
// but disjoint: unify(Int, Float) = ⊥ → no conflict
op + Int[n],   Int[n]   → Int[n]   ← ...
op + Float[n], Float[n] → Float[n] ← ...   // OK: no conflict
```

## 3.4 Orphan Rule

A hook or trait implementation is only valid if it is defined in the module
that defines **either** the operator/trait **or** at least one of the types
involved. Definitions satisfying neither condition are orphans and are rejected.

To implement an external trait for an external type, wrap the external type in a
`distinct` (see 02_TYPES.md §2.4) — this is the **newtype wrapper** pattern. The `distinct`
declaration creates a new nominal type owned by the declaring module. Ownership
of the `distinct` wrapper counts as ownership of a type involved in the
implementation: `implementation ExternalTrait MyWrapper` is valid because the
declaring module owns `MyWrapper`, even though neither `ExternalTrait` nor the
underlying wrapped type belongs to it. The wrapped type's identity is irrelevant
to the orphan check — only the `distinct` type's ownership matters.

**`distinct` types in the ambiguity check.** A `distinct` wrapper creates a
new nominal type. `unify(MyNumW, MyNum) = ⊥` follows directly from the
concrete-type disjointness rule in §3.3 (`unify(T1, T2) = ⊥` for `T1 ≠ T2`):
`MyNumW` and `MyNum` are different named types. Hooks for `MyNumW` and
`MyNum` are therefore structurally disjoint and never conflict — no special
handling is required.

**Specificity of `distinct` types.** `distinct` types are concrete types and
occupy the "concrete type" tier in §3.2's ordering. `distinct` does not
inherit trait implementations from the wrapped type; the programmer must
implement each trait explicitly. If `MyNumW` implements trait `C`, then
`op + MyNumW` is more specific than `op + (a : C)` by the standard
concrete-over-constrained-variable rule. If `MyNumW` does not implement `C`,
`unify(MyNumW, (a : C)) = ⊥` and there is no conflict. Either way, no
special case is needed.

## 3.5 Trait-Hook Unification

Traits and hooks are two views of the same mechanism. A `trait` declaration
names a bundle of hook signatures that a set of types must provide together.
An `implementation` block (SYNTAX.md §9.5) declares that a concrete assignment
of types to the trait's type variables satisfies all of those signatures,
enabling the compiler to verify completeness.

**Standalone hooks** (bare `op`, `uop`, `iter`, etc. at the top level) are
valid and define individual dispatch entries without claiming any trait
membership. Completeness is not checked. This is appropriate for one-off
operator definitions and non-trait extensions.

**`implementation` blocks** declare intent. The block must define all
**abstract** methods required by the named trait for the named type assignment;
methods that carry a default implementation in the trait body may be omitted
(the default is inherited) or overridden (the provided definition takes
precedence). Missing abstract methods are a compile error at the closing
delimiter. Both forms produce identical entries in the hook dispatch table
— the block adds completeness enforcement and documents the programmer's intent
but introduces no new runtime semantics.

Traits are declared with one or more type variable names after the trait name;
the `implementation` header supplies one concrete `type_expr` per variable,
positionally. Single-type traits (`trait Num a`) take one concrete type;
multi-type traits (`trait Add a b r`) take two or more. **Partial
instantiation is not permitted**: every type variable in the trait's parameter
list must be supplied with a concrete type in the `implementation` header. An
`implementation` that omits one or more type variables is a compile error. To
share an implementation across a family of types, use a constrained hook
(`implementation ∀ (a : Num) ⇒ Eq (List a)`) rather than a partial
instantiation.

**Supertrait constraints**: a `type_constraint` on a `trait_def` (`trait ∀ (a :
Functor) ⇒ Applicative a`) declares that any type implementing the
child trait must also implement all named supertraits. The compiler enforces
this at the `implementation` block: attempting to write `implementation
Applicative MyType` without a prior `implementation Functor MyType` is a
compile error. Supertrait methods are in scope inside the child trait's default
implementations and in `implementation` bodies — no explicit `type_constraint`
repetition is needed at call sites. The `type_constraint` on `impl_def`
retains its existing meaning (conditional implementation: `∀ (a : Ord) ⇒
implementation Eq (List a)`); the two uses are distinct by syntactic position.

**Default implementations**: a `sig` entry with a `'←' program` body provides
a default. Defaults are resolved at the `implementation` block: a method is
instantiated once for each concrete type assignment (whole-program
compilation). The compiled result is identical to an explicitly written
override — there is no runtime indirection through a vtable. Defaults may call
other methods of the same trait (including abstract ones); the compiler checks
that all abstract dependencies are satisfied before instantiating the default.

**Default resolution order in supertrait chains.** When an `implementation`
block does not override a method, the compiler searches the supertrait chain
for a default using **depth-first, left-to-right traversal with deduplication**
— each trait is visited at most once, at its earliest position in the traversal
order. The first default found is used. For a linear chain `A ← B ← C`, the
order is `C → B → A`: the most-derived default wins. For a DAG with multiple
supertraits (e.g., `trait C ← (Self : A) (Self : B)`), the traversal visits `C`
first, then `A` and its ancestors, then `B` and its ancestors (any already-visited
traits are skipped).

If no trait in the chain provides a default, the method is **required** — the
`implementation` block must provide it or the compiler reports an error naming
the missing method and the trait that declared it abstract.

**Worked example — diamond DAG:**

```
trait Shape a
  view .area : Self → Float ← _ → 0.0        // Shape default

trait ∀ (a : Shape) ⇒ Polygon a
  view .area : Self → Float ← _ → 1.0        // Polygon default (overrides Shape's)

trait ∀ (a : Shape) ⇒ Colorable a
  view .area : Self → Float ← _ → 2.0        // Colorable default (overrides Shape's)

trait ∀ (a : Polygon) (a : Colorable) ⇒ ColoredPolygon a
  // no default for area
```

DFS left-to-right traversal from `ColoredPolygon`:

1. `ColoredPolygon` — no default
2. `Polygon` — **default found (1.0); use this one**
3. `Shape` — visited; skip
4. `Colorable` — would have a default, but traversal already found one at step 2
5. `Shape` — already visited; skip

`Polygon`'s default wins. If the supertrait order were reversed —
`∀ (a : Colorable) (a : Polygon) ⇒` — the traversal would visit `Colorable`
at step 2 and its default (2.0) would win instead. **Declaration order in the
`∀` list is the tiebreaker for diamond cases.**

An `implementation ColoredPolygon MyShape` that does not override `area`
receives `Polygon`'s default. An implementation that provides `area` explicitly
overrides all defaults regardless of the traversal order.

**Derived defaults see concrete implementations.** When a default calls other
methods of the same trait (e.g., `'const ← x v → x 'map (_ → v)` calling the
abstract `'map`), those calls resolve against the **implementing type's concrete
hooks** — not against other defaults. The default is instantiated in the
context of the final `implementation`, so all sibling method calls dispatch as
if the programmer had written the default inline in the `implementation` block.

**Parameterized `Self`**: within a trait signature, `Self` refers to the
implementing type. When `Self` appears *applied* to trait variables — `Self T`,
`Self K V` — the implementing type must be a type constructor of matching arity.
`implementation Functor Tree` binds `Self = Tree`; the compiler substitutes
`Self T = Tree T` and `Self U = Tree U` throughout the method signatures and
kind-checks that `Tree : Type → Type`. This extends `Self` from a ground type
reference to a constructor reference, resolving the return-type error present in
any trait that maps elements to a different type (e.g., `iter 'map` returning
`Self` rather than `Self U`).

```
// Standalone: no completeness check, no trait membership claimed
op + MyNum, MyNum → MyNum ← ...
op - MyNum, MyNum → MyNum ← ...

// Single-type implementation block: completeness enforced, membership declared
implementation Num MyNum
  op  +  ← a b → ...
  op  -  ← a b → ...
  op  *  ← a b → ...
  uop -  ← a   → 0 - a           // negate
  from Int → MyNum ← n → ...     // literal polymorphism

// Multi-type trait: mixed-type addition
trait Add a b r
  bop + : a, b → r

// Multi-type implementation block: positional assignment a=Int, b=Float, r=Float
implementation Add Int Float Float
  bop + ← a b → prim.int_float_add a b

// Supertrait: Applicative requires Functor
trait ∀ (a : Functor) ⇒ Applicative a
  bop <*> : Self (T → U), Self T → Self U  // abstract: must be implemented

// Supertrait chain: Monad requires Applicative (and transitively Functor)
trait ∀ (a : Applicative) ⇒ Monad a
  bop >>= : Self T, (T → Self U) → Self U  // abstract

// Default implementation: 'const is derived from 'map (abstract in Functor)
trait Functor T
  iter 'map   : Self T, (T → U) → Self U   // abstract
  iter 'const : Self T, U → Self U         // default
    ← x v → x 'map (_ → v)

// Implementing Functor: only 'map is required; 'const is inherited
implementation Functor MyList
  iter 'map ← f mapper → ...              // 'const automatically available

// Overriding a default: provide a faster specialised version
implementation Functor Array
  iter 'map   ← f mapper → prim.array_map f mapper
  iter 'const ← x v     → prim.array_fill (x 'length) v
```

### 3.5.1 Container Traits

The retired `vec` and `dict` keywords are construction methods in a trait
hierarchy that separates the traversal contract from the indexing contract.
All container traits use parameterized `Self` (§3.5), which also corrects a
latent error in the original monolithic `Sequence` definition: `iter 'map`
must return `Self U`, not `Self`, since mapping over `Tree Int` with
`(Int → String)` produces `Tree String` — a different instantiation.

**Core traversal traits** — any parameterized container may implement these
independently:

```
trait Functor T
  iter 'map  : Self T, (T → U) → Self U        // type-changing element map

trait Foldable T
  iter 'fold : Self T, (A, T → A), A → A       // reduce with accumulator

// `arr 'fold f` (two arguments) is a valid partial application of type
// `A → A` via LTR application — the initial value is the remaining argument.
// `arr 'fold f init` is the fully applied form. There is no "first element
// as seed" semantics for `'fold`; homogeneous no-seed reduction (`fold1`)
// is a potential stdlib addition, not part of this trait.
```

The function type `(a →)` is itself a `Functor`: mapping over `(a → T)` with
`(T → U)` yields `(a → U)` — function composition. This instance grounds the
atop and fork rules (04_TRAINS.md §4.2):

```
implementation Functor (a →)
  iter 'map ← f mapper → x → x f mapper       // composition: (a→T), (T→U) → (a→U)
```

**`Sequence` trait** — indexed, ordered, constructible containers:

```
trait Sequence T
  from       : T[] → Self T                     // literal: [a;b;c]
  gen        : (A → Option (A, T)), A → Self T  // unfold from seed
  project    : Self T, Nat → T                  // index read: s[n]
  assign     : Self T, Nat, T → Self T          // index write: s.(n←v)
```

**`Mapping` trait** — key-value containers:

```
trait Mapping K V
  from       : (K, V)[] → Self K V             // literal: {k:v}
  project    : Self K V, K → Option V           // key lookup
  assign     : Self K V, K, V → Self K V        // key insert / update
  iter 'fold : Self K V, ((K, V) → A), A → A   // fold over pairs
```

`Array`, `List`, and `Vec` implement `Functor`, `Foldable`, and `Sequence`.
`Tree T` implements `Functor` and `Foldable` but not `Sequence` — there is
no natural positional index on a tree. `Set T` implements `Foldable` and
`Sequence` for construction (`[a;b;c] : HashSet`) but not `Functor` — element
mapping can violate the uniqueness invariant.

`HashMap K V` and `BTreeMap K V` both implement `Mapping`; the type annotation
selects the storage strategy. `[a;b;c] : Array`, `[a;b;c] : List` etc. trigger
the respective `Sequence.from` implementations.

### 3.5.2 Output-Dispatch: `from`, `gen`, and `iso`

`from` and `gen` select their implementation based on the **result type**
rather than an argument type (§3.1). The specificity ordering (§3.2) applies to
the result type pattern in the same way it applies to argument type patterns for
input-dispatch keywords. The definition-site ambiguity check (§3.3) runs on the
result type pattern. The orphan rule (§3.4) applies: the `from` or `gen`
hook must be defined in the module that owns either the result type or the
source/seed type.

`from` enables **literal polymorphism**: an integer literal `3` has an
underlying type of `Int`, but `3 : Float` selects the `from Int → Float`
hook, constructing a `Float`. A trait like `Num` requires `from Int → Self`
as one of its methods, making all `Num` implementors accept integer literals
directly.

**Ambiguity resolution**: when multiple `from Int → T` hooks are in scope
and the surrounding context does not fix `T` (e.g. `x ← 3`), the compiler
reports a type ambiguity error — it does not silently default to any one
hook. The programmer must supply a type annotation (`3 : Float`) or a type
ascription on the binding (`x : Float ← 3`) to resolve the ambiguity. There is
no numeric default type (`Int` is not a fallback). This is intentional: silent
defaults are a common source of precision bugs in numeric code.

`gen` enables **output-driven generation**: `seed 'generate f : MyList`
selects `MyList`'s `gen 'generate` implementation, unfolding `seed` into a
`MyList` using the generator function `f`.

**Generator contract — target type sets the shape.** The type of `f`
is determined by the result type via output-dispatch. The target collection's
`gen` hook declares what generator shape it expects; the type checker
enforces it at each call site. Two canonical shapes, standardised by the
`Sequence` trait:

```
// Finite structures (List, Array, etc.) — None terminates the unfold:
gen 'generate : seed, (seed → Option (a, seed)) → List a

// Infinite structures (Stream, etc.) — no termination condition:
gen 'generate : seed, (seed → (a, seed)) → Stream a

// Effectful variants — effects thread through naturally:
gen 'generate : seed, (seed → ⟪e⟫ Option (a, seed)) → ⟪e⟫ List a
```

The programmer writes `f`; its required type is inferred from the result type
context. Finite generators return `Option (value, newSeed)` — `None` signals
the end of the sequence, `Some (v, s)` emits `v` and continues with seed `s`.
Infinite generators return a plain pair `(value, newSeed)` and are typed
accordingly — the missing `Option` wrapper is the static signal that no
termination is intended. User-defined collection types follow the same pattern:
their `gen` hook declares the generator shape, and users implement that
shape.

**`from Field` — `from` coercion to accessor function**: A concrete field name
token (e.g. `.x`, type `Field`) converts to a record accessor function when the
output context demands a function type. The stdlib entry:

```
from ∀ (f : Field) T rest ⇒ f → ({f: T | rest} → T) ← prim.field_to_fn
```

This fires implicitly in tacit contexts where a `Field` token appears where a
function is expected (e.g. `arr 'map .x`, where `iter 'map` demands `a → b`).
`from` does not fire when the type context expects `Field` directly — field
arguments to `project`, `assign`, and generic `∀ (f : Field) ⇒` functions
receive the token without coercion. The `(.x)` section form is syntactic sugar
for `(from .x)`. See SYNTAX.md §5.3.

**`iso` — bidirectional coercion with round-trip laws**: `iso` declares both
directions of an isomorphism in a single block, ensuring that the two generated
`from` hooks form a true bijection. A single `iso` declaration desugars
into a forward `from A → B` hook, a backward `from B → A` hook, and a
round-trip law pair registered with the `Testable` protocol:

```
// iso declaration (block form; layout rule applies):
iso ∀ ⇒ Celsius, Float
  from Float → Celsius ← c → prim.celsius_wrap c
  from Celsius → Float ← c → prim.celsius_unwrap c

// Desugars to:
from Float → Celsius ← c → prim.celsius_wrap c
from Celsius → Float ← c → prim.celsius_unwrap c
// + Testable round-trip laws registered for both directions
```

The two round-trip laws are:
- **Forward then backward**: `(x from Float into Celsius) from Celsius into Float = x` for all `x : Float`
- **Backward then forward**: `(y from Celsius into Float) from Float into Celsius = y` for all `y : Celsius`

The `Testable` protocol generates property-based tests for these laws. In
addition, the compiler uses the declared inverse relationship to enable a
**cancellation rewrite**: `x from A → B from B → A = x` — a round-trip that
reduces to the identity and can be eliminated. This rewrite fires only when the
inverse pair was declared together via `iso`; standalone `from` declarations
carry no such guarantee, since the backward direction may not exist or may not
be the mathematical inverse.

The two `from` hooks generated by `iso` participate in normal
output-dispatch resolution (§3.5.2) and obey the same specificity ordering
(§3.2) and orphan rule (§3.4) as hand-written `from` declarations. At most one
`iso` block may exist for any ordered type pair `(A, B)` in a given scope.

### 3.5.3 Array and Table Combinators

The following `iter` hooks have signatures that are not obvious from the
generic container traits and are specified here.

**`'groupby`** — partition a collection by a key function:

```
iter 'groupby : T[n], (T → K) → HashMap K T[]
```

Group sizes are data-dependent; the inner array loses its dimension variable.
`T[]` (dynamic size) is correct — group membership is not statically
predictable. Arra uses **bag semantics** by default (duplicates preserved):
`'groupby` retains every element in its group, including repeated values.

**`'distinct`** — deduplicate, preserving one occurrence of each value:

```
iter 'distinct : (a : Eq)[n], () → ∃(m : Nat, m ≤ n) (a : Eq)[m]
```

Same sigma type as `'filter`: output size is bounded by input but unknown.
This is the explicit opt-in for **set semantics** (one occurrence of each
distinct value retained); all other array operations use bag semantics.
**Occurrence retained**: the **first** occurrence in traversal order is kept;
later duplicates are discarded. This is deterministic and independent of the
`Eq` implementation.

**`'asofJoin`** — nearest-prior-timestamp join:

```
iter 'asofJoin : T[n], U[m], (T → Timestamp), (U → Timestamp) → (T, Option U)[n]
```

For each row in `T`, finds the last row in `U` whose timestamp does not
exceed `T`'s timestamp. Returns `Option U` because no prior `U` row may
exist. This combinator is named because the pattern is common and the
implementation requires a merge-sort pass rather than a simple nested loop.
All other join types (left outer, anti-join, natural join) are derivable
from `'flatMap` + `'filter` + `Option` without named combinators.

**Heterogeneous maps**: `HashMap K V` requires a uniform value type `V`.
For structured heterogeneous data (known field names, mixed types), use a
row type: `{a: Int, b: String, c: Float}`. For genuinely open-ended
heterogeneous data, the stdlib provides `Any ← ∃ (t : Type) t`, usable as
`HashMap Symbol Any`. `Any` is an explicit opt-out of static typing for
that value; its use signals that the data structure is intrinsically dynamic.

### 3.5.4 `view` — Computed Read-Only Projections

`view` declares a computed, read-only projection that participates in
field-access syntax (`r.x`, `r .x`) and tacit pipeline chains. It dispatches
on the container type (left argument), exactly like `project`, but the
projection is intrinsically read-only — no corresponding `assign` exists and
none may be defined:

```
// Computed field: 'area' is not stored in Circle, but accessible as circle.area
view ∀ ⇒ Circle, .area → Float ← c → c.r * c.r * pi

// Computed field on a parametric type
view ∀ a ⇒ (a : Ord)[], .median → a ← arr → arr 'sort (arr.length / 2) app
```

Any expression that places a `view` name in an `assign` target is a type error:

```
circle.(area ← 5.0)   // type error: 'area' is declared view, not assignable
```

**Dispatch**: input dispatch on the container type, identical to `project`. A
type should not declare both a `project` and a `view` for the same field name
— the compiler emits an error at the definition site if both exist for the same
container type and field name.

**Tacit use**: `view` projections participate in tacit pipeline chains and in
the `from Field → function` coercion (§3.5.2) exactly like stored fields:

```
circles 'map .area              // .area coerces to (Circle → Float) via from Field
circles 'filter (.area > 10.0)
```

## 3.6 User-Defined Symbolic Operators

Users may define operators using any Unicode code point that is a valid
`op_char` (see SYNTAX.md §1.2). User-defined operators are semantically
identical to built-in operators: they are hooks dispatched by the standard
specificity ordering (§3.2), participate in the orphan rule (§3.4), and may
appear in sections `(÷)` or infix projection chains `arr ÷ 2`.

**There is no new semantic machinery.** A user-defined symbolic operator is an
hook whose name happens to be a Unicode character rather than an ASCII
sequence. The desugaring from symbolic form to hook call is the same as
for all operators.

**Watcher rule semantics**: the `Watch` attribute on a hook definition
is a source-level annotation. It has no runtime semantics and does not affect
hook dispatch. Its sole effect is to register a file-watcher substitution
rule that becomes active when the definition is in scope. The rule is scoped
to the import: excluding the definition (via `hiding` or selective `open`)
also suppresses the watcher rule.

**Uniform precedence, no declarations**: all operators — built-in and
user-defined — share equal precedence and associate strictly left-to-right.
`a + b * c` parses as `(a + b) * c`. There is no mechanism to declare a
custom precedence level or associativity. This is a deliberate consequence
of the APL heritage.

## 3.7 Pattern Hooks (`pat`)

`pat` hooks define user-extensible patterns. A `pat` hook is a
function `T → Option U` (or `T → Option ∃(x, constraint) U` for a
Z3-witnessed form) that appears in pattern position in match branches and
binding expressions. The extraction semantics:

- **Success** (`Some u`): the branch matches; `u` is bound by the
  sub-pattern in the branch head.
- **Failure** (`None`): the branch does not match; the value is either
  covered by another branch or the match fails (see exhaustiveness below).

Syntactically, `pat` hooks use `upper` names and are applied via
`upper pattern?`, identical to constructor patterns. The type checker
resolves whether an `upper` name is a constructor or a `pat` hook.

**Subsumption of constructors**: a data constructor `C : T → U` is
semantically a `pat` hook whose extraction function is the inverse of
the construction. The unified treatment is: constructors are built-in
`pat` hooks with total extraction (returning `Some` for exactly the
values constructed by `C` and `None` otherwise).

**Z3 refinement via sigma return types**: when a `pat` hook's return
type is `Option ∃(x : T, φ) U`, the constraint `φ` is added to the Z3
context at the binding site. Inside the matching branch, the bound variable
carries a refined type. Example:

```
pat Positive : Int → Option ∃(k : Nat, k > 0) Int ← n → (n > 0) (True → Some n; False → None)

x
  Positive k → k         // Z3 knows k > 0; e.g. can safely subtract 1
  _          → 0
```

**Disjointness and unordered semantics**: match branches are unordered —
the compiler may evaluate them in any order or in parallel. This requires
named branches to be **pairwise disjoint**: no input value may match more
than one. Disjointness is verified by Z3 using sigma constraints. If two
`pat` branches carry mutually exclusive Z3-linear constraints (e.g. `k > 0`
and `k < 0`), they are proved disjoint. If a `pat` hook has no sigma
constraint (plain `Option T`), Z3 cannot prove it disjoint from overlapping
branches — placing it alongside another branch that could match the same
value is a **compile error**.

**`_` as residual default**: a `_` branch carries the implicit constraint
"complement of all other branches." It is automatically disjoint from all
named branches because it matches exactly what they do not. A `_` branch
may appear in any match alongside any combination of structural and `pat`
branches.

**Exhaustiveness and `⟪Fail⟫` introduction**: exhaustiveness is checked in
two tiers:

1. **Structural tier**: for `type` definitions, standard constructor coverage
   analysis. Exhaustive iff all constructors are covered (up to wildcards).
2. **Z3 tier**: for `pat` hooks with sigma return types, Z3 checks
   whether the branch constraints together cover the full domain of the
   scrutinee type.

If neither tier proves exhaustiveness, the match type is `T → ⟪Fail⟫ U` —
the function carries `⟪Fail⟫` on its arrow, which propagates through the
effect system and must be handled or forwarded at call sites. No compile
error is raised for inexhaustiveness.

A `_` branch is **never required**. Its presence with a body makes the
match total, removing `⟪Fail⟫` from the type. The compiler emits a
**warning** (not an error) when structural analysis finds visibly missing
constructors from a `type` definition. No warning is emitted for `pat` hooks
or `when` guards where Z3 is the only possible judge — the `⟪Fail⟫` on the
type is the sufficient signal.

**Orphan rule**: `pat` hooks follow the same orphan rule as all
hooks (§3.4). A `pat` hook for an external type must be defined in
a module that owns the operator or the type.

**`when` guards**: a `when expr` guard is **not** desugared to a `pat`
hook. Guard conditions are Z3 propositions: `expr` is injected directly
as a hypothesis into the branch body's constraint context. Disjointness
between guarded branches is checked per match expression — Z3 tests whether
`cond_i ∧ cond_j` is satisfiable for each pair (i, j); if satisfiable, the
branches overlap and the compiler rejects the match. Non-Z3-linear guards
(e.g. `when isPrime k`) cannot be proved disjoint; two such guards in the
same match is a compile error unless one is subsumed by a `_` residual.
(See SYNTAX.md §5.3 for surface syntax.)
