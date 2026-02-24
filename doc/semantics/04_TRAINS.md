# 4. Trains and Tacit Programming

## 4.1 Atop (Function Composition)

Application of a function to a function with congruent types is function
composition. The grammar is already quasi-concatenative: `f g` where both `f`
and `g` are functions means `g ∘ f` (apply `f` first, then `g`). This gives
atop (2-train) for free.

## 4.2 Forks (Operator Lifting)

Operator application is **universally lifted over function types**. When an
operator appears between two functions, the result is a new function:

```
x (f ⊕ g)         ≡  ⟦x f; x g⟧ ⊕              // monadic: f, g unary
⟦x; y⟧ (f ⊕ g)   ≡  ⟦⟦x; y⟧ f; ⟦x; y⟧ g⟧ ⊕  // dyadic: f, g binary
```

**All operators participate — including user-defined ones.** No per-operator
definitions are required.

**Pipeline**: the parser always produces a `sym_proj` AST node for `f op g`
regardless of whether `f` and `g` are functions — the parser has no type
information. The type checker then applies fork detection as a rewrite phase:

1. **Parse**: `f op g` → `sym_proj(f, op, g)`.
2. **Type inference**: infer types of both operands via bidirectional
   inference as usual.
3. **Fork check**: if both operands have function types with a **common
   domain** — `f : a → b` and `g : a → c` where `a` unifies — rewrite
   `sym_proj(f, op, g)` to `liftA2(op_section, f, g)`.
4. **Hook dispatch**: runs on remaining (non-rewritten) `sym_proj` nodes,
   and on the inner `op` call site inside `liftA2`'s body (resolved by
   whole-program monomorphisation).

The "before hook dispatch" phrasing means step 3 precedes step 4 — both
happen inside the type checker. Fork detection requires knowing operand types,
so it is always after type inference (step 2).

**Ambiguity resolution — eager detection with annotation fallback**: fork
detection is *eager*. Step 3 fires only when both operands have *fully known*
function types at the point of checking — i.e., neither operand's type is an
unresolved unification variable. If either operand type is still a variable
(resolvable only from downstream context), fork detection is skipped and
hook dispatch proceeds on the `sym_proj` node as-is.

This is the correct behaviour for the ambiguous case: a program where `f`'s
function-ness is only inferrable from the result type of `f op g` is genuinely
ambiguous — it is impossible to determine without speculation whether fork or
hook dispatch was intended. The programmer resolves it with a type
annotation or an explicit `liftA2` call:

```
// Ambiguous: f's type is a variable at this point — fork does not fire,
// hook dispatch proceeds, likely a type error.
someOp f g

// Resolved with annotation — fork fires
(f : Int → Bool) + g

// Resolved with explicit liftA2 — always forks regardless of inferred types
⟦(+); f; g⟧ liftA2
```

The compiler emits a targeted diagnostic when hook dispatch fails on
function-typed operands that *could* have been a fork: "fork not detected for
`f op g`; if a fork was intended, annotate the operand type or use `liftA2`
explicitly."

**Common domain condition**: both operands must unify to function types `a → b`
and `a → c` with the *same* domain `a`. If domains do not unify, no fork is
detected and hook dispatch proceeds on the operands as-is (a type error if
no `op` hook exists for function-typed values).

**Recursion — dyadic case**: `f op g` where `f, g : a → b → c` has domain `a`.
Fork fires: `liftA2 op f g : a → ((b→c) op_result (b→c))`. When applied to
`x`, this yields `op (f x) (g x)` where both `f x, g x : b → c` — fork fires
again at the inner `op`. The dyadic case emerges automatically from this
recursive structure; no special handling is required.

**LTR chains**: `f + g + h` parses as `(f + g) + h`. If all three are
functions with domain `a`:
- `f + g` forks → `liftA2 (+) f g : a → r`
- `(liftA2 (+) f g) + h` — result is `a → r`, `h : a → s` — forks again →
  `liftA2 (+) (liftA2 (+) f g) h`
- Applied to `x`: `(f x + g x) + h x` ✓

**`op` as first-class value**: the rewrite passes `op` as a constrained
polymorphic section — `(op) : ∀ b c d ⇒ (b, c → d)`. The specific hook
of `op` is not chosen at the fork rewrite site; it is deferred to the inner
call site `⟦x f; x g⟧ op` inside `liftA2`'s body. Whole-program compilation
monomorphises this call site with the concrete types of `f x` and `g x`,
resolving the correct hook there. The fork rewrite and hook dispatch
are therefore independent: fork does not "capture" a hook choice, it
only restructures the call.

**`liftA2` is a real stdlib function**, not a compiler fiction:

```
liftA2 : (b, c → d), (a → b), (a → c) → (a → d)
liftA2 ← h f g → x → ⟦x f; x g⟧ h
//                    ^^^^^^^^^^^^ call vector: passes (f x) and (g x) to h
// example: liftA2 (%) min max → x → ⟦x min; x max⟧ (%)
//                                 = x → (min x) % (max x)
```

A programmer may call `⟦(+); f; g⟧ liftA2` explicitly — it produces identical
code to the fork rewrite of `f + g`. Error messages from the rewrite site and
from explicit `liftA2` calls look the same.

This is the **reader Applicative** instance for `(a →)`. Atop (§4.1) is the
corresponding `Functor (a →)` instance — function composition — which also
means unary operators lift over functions via `iter 'map`:

```
⊖ f  ≡  f 'map ⊖  =  x → ⊖ (x f)              (when f : a → b)
```

```
mean  : sum % count      // monadic: (sum x) % (count x)
clamp : min % max        // dyadic:  (min x y) % (max x y)
```

**Opt-out**: to apply an operator to two function *values* without forking,
use a call vector — `⟦f; g⟧ op` (see SYNTAX.md §5.5). Call vectors are
explicit calls that bypass
`sym_proj` entirely; fork detection never fires. This is the natural escape
hatch when an `op` hook for function-typed arguments is genuinely intended.

Unlike BQN, forks are not detected syntactically. They emerge from the
type-checker rewrite and the quasi-concatenative grammar. User-defined
operators participate automatically; no `/'-Forkable-'/` annotation or stdlib
registration is needed.

## 4.3 Tick Modifier Sections and Point-Free Pipelines

Tick modifier sections (SYNTAX.md §5.4) are first-class functions. A right
section `('map f)` has type `a[n] → b[n]` when `f : a → b`; it is exactly
the function `arr → arr 'map f`. All four section forms are valid for any
tick modifier except `'handle` (which has its own restriction — see SYNTAX.md
§5.4).

Because tick sections are ordinary functions, they compose via `∘` (atop,
§4.1) without requiring an explicit array argument:

```
('map f) ∘ ('map g)   // arr → arr 'map g 'map f
('sort) ∘ ('map f)    // arr → arr 'map f 'sort
```

This means any pipeline can be expressed either:
- **Point-free** (composing sections): `('map parse) ∘ ('filter valid) ∘ 'sort`
- **Pointed** (explicit subject): `arr → arr 'map parse 'filter valid 'sort`

Both forms are semantically identical. The point-free form is useful when
passing a pipeline as an argument or storing it as a named value:

```
normalise ← ('map toFloat) ∘ ('filter (x → x > 0))
data 'each normalise
```

The key algebraic law enabling map fusion follows directly:

```
('map f) ∘ ('map g)  =  'map (f ∘ g)   // two maps compose into one
```

Or equivalently for any `arr`:

```
arr 'map g 'map f  =  arr 'map (f ∘ g)
```

The compiler uses this (and the fold/scan variants below) as unconditional
rewrites before code generation.
