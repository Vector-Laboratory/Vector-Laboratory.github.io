# 7. Quoted Programs and Compile-Time Computation

## 7.1 `Program T` — Quoted Expressions

`` `[expr] `` quotes `expr` as an unevaluated program form. The result has type
`Program T` where `T` is the type of `expr` in the current scope. `Program T`
is a first-class stdlib algebraic type — a typed representation of an Arra
expression. It can be stored, passed to functions, returned, and pattern-matched
like any other value. There is no `macro` keyword and no distinct expansion
phase.

`.(expr)` splices `expr : Program T` into a `T`-typed hole. A splice at a
non-quoted program position is evaluated at compile time:

```
double : Program Int → Program Int
double ← e → `[.(e) + .(e)]

result : Int ← .( double `[3] )     // compiles to: result ← 3 + 3
```

**Free variable references** in a quoted form are resolved at the quotation
site — the same bindings visible at the `` ` `` call. Splicing the form
elsewhere does not change which bindings those names refer to. This is
already lexically safe by construction.

**Introduced names** (let bindings via `←`, lambda parameters, match-branch
bindings) carry no hygiene guarantee. A name introduced inside a quoted form
lands literally in the scope of the splice site; name capture is the
programmer's responsibility. The stdlib provides:

```
gensym : String → ⟪CompileTime⟫ Name
```

`gensym` returns a guaranteed-fresh name token usable in quoted forms. Freshness
is **global within a single compilation**: no two `gensym` calls within the
same compilation unit produce the same name, regardless of which function or
module calls it.
Use it when a code-generating function introduces temporaries that must not
conflict with names at the call site:

```
withCache : Program a → Program a
withCache ← e →
  tmp ← gensym "cached"
  `[.(tmp) ← .(e); .(tmp)]
```

For the primary use cases — type providers, hook generation, expression
templates that do not introduce new bindings — hygiene is not a concern.

## 7.2 Compile-Time Evaluation

Any expression appearing inside `.( )` is evaluated at compile time.

**Quoting is effect-agnostic.** `` `[expr] `` is well-formed regardless of
`expr`'s effect. `Program (⟪IO⟫ T)` is a valid type — it represents a
quoted IO-performing computation, not an IO operation itself. Constructing
and transforming `Program (⟪effect⟫ T)` values at compile time is pure;
the enclosed effect fires only when the generated code runs at runtime.

**Splice-site effect rules** depend on where the splice appears:

- *Top-level splice* (`.( )` at module scope, outside any expression):
  the expression must be pure or `⟪CompileTime⟫` and must produce
  `Program Declaration` or `Program Declaration[]`. Runtime effects are
  not permitted.

- *Expression-level splice* (`.( )` inside a definition body): the effect
  of the splice expression must be compatible with the surrounding context's
  effect row. A `Program (⟪IO⟫ T)` may be spliced in an `⟪IO⟫`-typed
  body; a pure `Program T` may be spliced anywhere.

```
// Fine: generating and splicing an effectful expression in matching context
makeLogger : String → Program (⟪IO⟫ ())
makeLogger ← msg → `[printLine .(msg)]

main : ⟪IO⟫ () ←
  .( makeLogger "hello" )
  .( makeLogger "world" )

// Rejected: effectful expression at a top-level declaration splice
.( makeLogger "hello" )    // error: top-level splice requires pure or ⟪CompileTime⟫
```

Because Arra uses whole-program compilation, there is no stage restriction: a
function producing `Program T` values may be defined in the same module where
its results are spliced. The compiler evaluates splices according to the
three-pass phase model described in §7.6.

```
// Code-generating function defined and used in the same module:
applyN : Int → Program (a → a) → Program (a → a)
applyN ←
  0 _ → `[x → x]
  n f → `[x → .(f) (.( applyN (n-1) f) x)]

composed : Int → Int ← .( applyN 3 `[(+1)] )
// expands to: composed ← x → (+1) ((+1) ((+1) x))
```

## 7.3 `Program T` as a Stdlib Type

`Program T` is an algebraic data type in the stdlib. It represents the Arra
expression AST and exposes constructors and combinators for inspecting and
building program forms. Compile-time functions that inspect the structure of
their inputs pattern-match on `Program T` directly.

Because types and values share the same grammar, type expressions are quoted
with the same mechanism:

```
`[Int]            // : Program Type
`[a[n] → b]       // : Program Type
`[arr 'map f]     // : Program b[n]   (given arr : a[n], f : a → b)
```

Three invariants govern the `Program T` representation:

**Hooks are resolved at quote time.** When a quoted expression contains
a hook application — `x + y`, `arr 'map f`, `r.field` — the specific
hook selected by the type checker at the quotation site is embedded in
the AST node. Pattern-matching on a `Program T` value can distinguish `Int`
addition from `Float` addition. The `Program T` value is self-contained; no
re-resolution occurs at splice time.

This is a deliberate design decision, not an omission. `Program T` is a
**semantic value**: the embedded hook represents the library's chosen
operation, not a re-dispatchable call. This is analogous to how a closure
captures its lexical environment — splicing a `Program T` elsewhere does not
rebind its hook choices any more than calling a closure rebinds its free
variables. A consequence: if the splice site has a more-specific hook for
the same operator (e.g., a user-defined `op + Int, Int` alongside the library's
`op + (a : Num), (a : Num)`), the embedded hook is used, not the
more-specific one. This is correct — the library's semantics are preserved —
and always type-safe, since the embedded hook was already checked against
`T`. A programmer who needs generated code to use the splice site's hook
set must construct a new `Program T` at the splice site using the desired
hooks directly.

**Binding forms use named variables.** Lambda parameters, let-bound names
(`←`), and match-branch bindings are represented as literal string names in
the AST, not de Bruijn indices. This makes hand-constructed ASTs readable
and makes pattern-matching on variable names straightforward. The hygiene
consequences are described in §7.1.

**Types are available on request, not eagerly embedded.** The `Program T`
parameter captures the overall type at the top level. Individual
subexpression types are not stored in every AST node; they are computed on
demand via:

```
typeOf : ∀ T ⇒ Program T → Program Type
```

Most transformers operate on structure (literal, variable, application,
lambda) rather than needing the type of every subnode. `typeOf` is available
when precise type information is needed for a subexpression.

The full set of `Program T` ADT constructors — literals, variable
references, applications, lambda forms, hook application nodes, splice
forms, and type-level forms — is specified in the stdlib documentation. The
three invariants above are the semantic commitments; the exact ADT shape is
an implementation detail of the stdlib.

## 7.4 Relationship to the Type System

The type of `` `[expr] `` is `Program T` where `T` is inferred from `expr`.
Polymorphic quoted programs use the standard `∀` quantification:

```
identity_prog : ∀ a ⇒ Program (a → a)
identity_prog ← `[x → x]
```

Splicing `.( p : Program T )` into a `T`-typed position typechecks directly.
The type of the spliced form propagates into the surrounding expression as if
the programmer had written it inline.

## 7.5 Top-Level Splices and `Program Declaration`

A `.( )` splice at module top-level (outside any expression context) may
produce a `Program Declaration` value or a `Program Declaration[]` array. The
declarations are inserted into the module scope at the splice site, exactly as
if they had been written inline.

`Program Declaration` is a stdlib algebraic type representing any top-level
definition form: `type` definitions, `alias` synonyms, hook definitions,
`derived` declarations, trait implementations, or further nested top-level
splices. The stdlib provides constructors for each form.

```
// Type provider: fetch schema at compile time, emit data declarations
.( fetchSchema "postgres://localhost/mydb" 'flatMap schemaToDecls )

// Programmatic hook generation
.( fieldNames 'flatMap (f → `[project MyType, .(f) → String ← prim.get]) )
```

Names introduced by a top-level splice are visible only to code textually
following that splice. This is the only source-order dependency in an otherwise
order-independent module body.

## 7.6 Phase Ordering

Top-level splices impose a defined evaluation order within module compilation.
The compiler processes each module in three passes:

**Pass 1 — Type-check all non-splice definitions.** Every value binding,
hook, `type` definition, and trait implementation that does not contain a
top-level splice is type-checked. Splice functions — functions whose results
will be spliced — are also type-checked in this pass. The compiler knows the
return type of every splice (`Program T` or `Program Declaration`) before
evaluating any splice.

**Pass 2 — Evaluate top-level splices in source order.** Each `.( expr )` at
top-level is evaluated. `expr` must be pure or `⟪CompileTime⟫`. The generated
`Program T` or `Program Declaration[]` values are inserted at the splice site.

**Pass 3 — Type-check generated declarations.** Declarations produced by
splices are type-checked. If generated declarations contain further top-level
splices, passes 2 and 3 iterate until no new splices remain or a cycle is
detected (compile error).

**No stage restriction**: whole-program compilation makes all definitions
visible in Pass 1. Splice functions may be defined in the same module as their
use sites — no pre-compilation step is required.
