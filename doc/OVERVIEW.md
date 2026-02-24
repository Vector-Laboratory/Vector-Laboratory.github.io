# Arra — Overview

Arra is an array-functional language built on a single observation: APL's core mechanisms — rank polymorphism, implicit iteration, even function application itself — are all instances of type-directed dispatch. Making that dispatch explicit converges, through successive generalization, on the optic hierarchy. The surface expression of this is Arra's hook keyword system: eleven keywords, each naming a syntactic dispatch position, each corresponding to a category in the optic lattice.

## The Generalization Arc

Rank polymorphism in APL is implicit: the rank of an argument determines how an operation is applied, with each verb carrying its own rank behavior. The observation that unlocks Arra's design is that this is dispatch — the same mechanism as hook resolution in a typed language, just unacknowledged. The natural move is to bring it under the type system: array types carry their dimensions, dispatch on those types selects the iteration strategy, and rank polymorphism becomes a consequence of hook resolution rather than a parallel mechanism.

The same move applies to iteration more broadly. A function in fold position means something different from a function in map position. Arra makes this explicit with `iter` and `gen`, naming the fold and unfold dispatch positions respectively.

Function application is the surprising case. Juxtaposition in most languages is the one thing that is *not* hookable — the fixed primitive from which everything else is built. APL already breaks this: juxtaposition is rank-polymorphic and context-sensitive. Arra goes further and treats application itself as a dispatch site. The `app` keyword makes juxtaposition a hook site: the callable type on the left determines which hook fires. As far as we know, this is the only language in which whitespace is a user-hookable operator.

Once application is a dispatch site, the pattern becomes visible everywhere. Field read is dispatch on the container type — `project`. Field write adds a value — `assign`. Coercion is dispatch on the target type — `from`. Pattern matching is dispatch on the scrutinee type — `pat`. Each is a named syntactic position; each corresponds to an optic. The keyword table is not a list of features. It is the claim that these positions are necessary and sufficient.

## The Keyword Table

| Optic | Keyword | Direction | Syntactic position |
|-------|---------|-----------|-------------------|
| — | `uop` | input | postfix operator |
| — | `bop` | input | infix operator |
| Prism | `app` | input | juxtaposition — callable type left |
| Prism | `pat` | input | pattern position |
| Lens | `project` | input | field / index read |
| Lens | `assign` | input | field / index write |
| Getter | `view` | input | computed field, no setter |
| Traversal | `iter` | input | fold — function argument |
| Traversal | `gen` | **output** | unfold — function argument |
| Iso | `from` | **output** | coercion, one direction |
| Iso | `iso` | **output** | coercion, both directions + round-trip laws |

Nine of the eleven keywords dispatch on input type — the type of what is already in hand. The three exceptions (`gen`, `from`, `iso`) dispatch on the result type: the hook is selected by what the surrounding context demands, not what is supplied. This output polymorphism is how literal polymorphism, type-directed generation, and zero-cost coercions work without annotation at every use site.

The dual pairs — `app`/`pat`, `project`/`assign`, `iter`/`gen` — are the optic get/set or fold/unfold directions. `view` is intentionally unpaired; a Getter has no set direction by definition. `from` and `iso` are a half-pair: `from` declares one direction, `iso` declares both and registers the round-trip laws with the test framework.

`uop` and `bop` sit outside the optic hierarchy — they are pure arity forms for unary postfix and binary infix operators. They are in the table because they share the same dispatch and specificity machinery as everything else; they just do not correspond to a named optic category.

## What Follows

The hook keyword system is the core. The rest of the language fills in the consequences.

**Algebraic effects.** Type-directed dispatch requires that the type system track everything dispatch depends on. Effects — IO, state, failure, nondeterminism — are row-typed and propagate through the hook system automatically. The `iter` keyword is effect-polymorphic: `'map` over an effectful function produces an effectful result, with threading determined by whether the effect is `Distributable` or not. This is not a separate design decision; it is what happens when a serious effect system meets a serious hook system.

**Coherence.** Hook resolution requires a coherence story. Haskell uses orphan rules; Scala uses implicit search with priority. Arra uses whole-program compilation with import-boundary finalization: each call site resolves hooks using only the scope of the containing module. Downstream modules cannot retroactively affect upstream resolution. Stronger guarantee, stronger compilation requirement.

**Fusion.** Once `iter` and `gen` are typed dispatch positions, the compiler can reason about fusion algebraically. A chain of `iter` operations composes under rewrite rules derivable from the optic laws. SOAC fusion falls out of the hook structure rather than requiring a separate analysis pass.

**Derived views.** `derived` is the reactive analog of `iter`. When the source of a fold changes by a delta, the result changes by a corresponding delta. The compiler analyses which `⟪State⟫` cells a view depends on, registers delta functions for each `iter` hook in the chain, and maintains the result incrementally. Delta purity is enforced by the same type system that governs everything else.

**Dimension types.** If rank polymorphism is dispatch on array types, those types need to carry dimension information. Dimension variables are type variables of kind `Nat`, checked by Z3 rather than unification. Graceful degradation — hard error at the failing site, continued checking elsewhere with dynamic size `a[]` — keeps the system practical without sacrificing guarantees where they hold.

## Honest Debts

Arra draws heavily from prior work. The following is a precise accounting.

**APL, J, K, Q** — the array model, rank polymorphism, symbolic operators, tick-prefixed modifiers, and the columnar data orientation are all inherited from the APL lineage. Arra adopts LTR evaluation order rather than the traditional RTL, following Q's pipeline style but making it the universal rule rather than a convention.

**Haskell** — traits are typeclasses. The `∀ ⇒` constraint syntax, kind system, and row polymorphism are all Haskell-derived. The specificity ordering in hook resolution is a restricted form of instance resolution.

**Koka** — the algebraic effect system with row-typed effects, the `ctl`/`op` distinction between non-resumable and resumable operations, and the effect-polymorphic function types are taken directly from Koka.

**Futhark** — the SOAC fusion taxonomy and the use of dimension variables in array types as a practical (rather than purely theoretical) tool both draw on Futhark's design.

**Idris / dependent type theory** — sigma types for existentially quantified array bounds, and the general idea that dimension arithmetic is a type-level concern checkable by an SMT solver.

**OCaml** — the interface/implementation module split.

**Mercury** — nondeterminism as a first-class effect, and the `/'-Tabled-'/` annotation for tabled execution of recursive predicates.

**Uiua** — the file watcher approach to Unicode: write ASCII, get Unicode on save, keep both valid.

## Motivation

Design arguments are easier to evaluate against concrete code. The following example is a columnar database query with incremental maintenance — the kind of program Arra is aimed at.

```
type Trade ← {sym: Symbol, price: Float, vol: Int}

derived bestBySymbol ← db.trades
  'filter  (t → t.vol > threshold)
  'groupby .sym
  'map     (grp → grp 'map .price 'max ())
```

`bestBySymbol` is a `HashMap Symbol Float` — the best price per symbol among trades above a volume threshold. It reads almost identically to what you would write in K or Q. The difference is what the compiler has verified underneath.

`.sym` and `.price` are field accesses that work identically whether `db.trades` is stored row-oriented (`Trade[n]`) or columnar (`{sym: Symbol[n], price: Float[n], vol: Int[n]}`). The layout is a type annotation; the code does not change. The `from` coercion between the two representations is zero-cost and compiler-generated.

`.sym` in `'groupby` position is a `Field` token coerced to an accessor function by the `from Field → function` hook. No explicit lambda needed.

`'map` inside `'map` works on the grouped structure without rank annotation. Dispatch selects the correct iteration strategy from the types.

The entire chain is `⟪State⟫`-typed. Effects propagate out through the hook system automatically — no explicit threading, no monadic bind.

`derived` means `bestBySymbol` is not recomputed on every access. The compiler registers delta functions for `'filter`, `'groupby`, and `'map`, and maintains the result in O(delta) as `db.trades` changes. The delta functions are guaranteed pure by the type system; no locking is required.

The surface is familiar. The substrate is new.
