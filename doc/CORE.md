# Arra — Core IR

Core IR is the shared intermediate language between the frontend and both backends.
The type checker's primary job is to produce well-typed Core; the treewalk
interpreter and compiler backend both consume identical Core, making Core the
QCheck boundary between them. Every compiler pass operates on Core and must
preserve its typing invariants — Core is type-checked at each phase boundary.

---

## 1. Invariants

By the time a program reaches Core, the following hold unconditionally:

- **All hooks resolved.** Every `bop`, `uop`, `iter`, `gen`, `app`, `project`,
  `assign`, `view`, `pat`, `from` site has been resolved to a direct reference to
  a specific implementation. No dynamic dispatch tables exist.
- **All sugar eliminated.** Field access (`r.x`), record update (`r.(x ← v)`),
  `iso` (desugared to two `from` bindings), `derived` (desugared to delta stubs),
  and tuple index (`t.1`) are gone; only primitive projections and explicit
  construction remain.
- **All macros expanded.** Program T expansion is complete; no splice nodes remain.
- **Types explicit on every binder.** Every lambda, let, and case binder carries
  its type annotation. Core is fully typed and can be type-checked independently
  of the frontend.
- **Dimensions explicit.** Array types carry their dimension expressions (`a[n]`).
  Z3 has already verified all dimension constraints; Core carries the dimension
  structure but not proof witnesses (see Open Questions §8.1).
- **Effects explicit.** Effect rows appear in function types; `Perform` and
  `Handle` nodes appear in the term structure wherever effects are introduced or
  discharged.
- **Modules erased.** All names are fully qualified. The module system, `open`,
  and `interface`/`implementation` distinctions are gone.

---

## 2. Terms

```ebnf
expr = Var     name                            (* variable reference *)
     | Lit     literal                         (* int, float, string, bool, char *)
     | Lam     (name : type) expr              (* lambda; binder carries type *)
     | TyLam   tvar expr                       (* type abstraction: Λa. e *)
     | App     expr expr                       (* application *)
     | TyApp   expr type                       (* type application: e [T] *)
     | Let     (name : type) expr expr         (* non-recursive let *)
     | LetRec  { (name : type) = expr }+ expr  (* mutually recursive group *)
     | Case    expr type { branch+ }           (* case; return type explicit *)
     | Con     name expr*                      (* constructor applied to args *)
     | Tuple   expr+                           (* tuple construction, 1-indexed *)
     | Record  { name = expr }+                (* named record construction *)
     | Array   expr* dim                       (* array literal with dim annotation *)
     | ProjI   expr int                        (* tuple index: e.1, e.2, … *)
     | ProjF   expr name                       (* record field: e.x *)
     | Perform name expr*                      (* effect operation *)
     | Handle  expr handler                    (* effect handler installation *)
     | Prim    prim expr*                      (* primitive operation *)
     | Ann     expr type                       (* type annotation — survives erasure *)
```

`Case` carries its return type explicitly so passes can read it without
re-inferring. `LetRec` collects all mutually recursive bindings into one node.
`Ann` is kept through all Core passes; backends may erase it during emission.

---

## 3. Types

```ebnf
type = TVar    tvar                 (* type variable: a, b, f *)
     | TCon    name                 (* type constructor: Int, Bool, … *)
     | TApp    type type            (* type application: Option a *)
     | TFun    type type            (* pure function: a → b *)
     | TEffFun type eff type        (* effectful function: a → ⟪e⟫ b *)
     | TTuple  type+                (* tuple: (a, b, c) *)
     | TRec    { name : type }+     (* record: {x: Int, y: Float} *)
     | TArr    type dim             (* array: a[n] *)
     | TForall tvar kind type       (* ∀ a : K ⇒ T *)
     | TSigma  name type prop type  (* ∃(x : T, P) U — bounded existential *)
     | TEffect eff                  (* effect row: ⟪e | rest⟫ *)
```

`TRec` and `TTuple` are distinct — records are not normalised to tuples.
Both survive through all Core passes; `ProjI` indexes tuples, `ProjF` accesses
record fields.

---

## 4. Dimensions

```ebnf
dim = DVar name     (* dimension variable: n, m *)
    | DLit int      (* literal: 0, 5, 128 *)
    | DAdd dim dim  (* n + m *)
    | DMul dim dim  (* n * m *)
    | DDiv dim dim  (* n / m  — integer division *)
    | DSub dim dim  (* n - m *)
```

Dimension expressions are preserved in Core for two reasons: the treewalk
inserts `prim.assert_dim` checks at validated sites, and the compiler uses
dimension information during fusion to avoid materialising intermediate arrays.
Z3 has signed off on all constraints; Core carries the arithmetic structure
without proof certificates.

---

## 5. Effects

```ebnf
eff = EffEmpty                 (* ⟪⟫ — no effects, pure *)
    | EffVar   name            (* effect variable: e *)
    | EffCon   name type* eff  (* labelled effect: Fail T | rest *)
```

```ebnf
handler = { handler_clause+ }

handler_clause
    = HCtl    name (name : type) expr                (* ctl op (x : T) → body  — non-resumable; no continuation *)
    | HOp     name (name : type) (name : type) expr  (* op  op (x : T) (k : T → ⟪rest⟫ a) → body  — resumable *)
    | HReturn (name : type) expr                     (* return (x : T) → body *)
```

`HCtl` does not receive a continuation — the handler body must diverge or escape
via another `Perform`. `HOp` receives `k : result → ⟪rest⟫ a`; the body may
invoke it zero or more times. `Handle` discharges exactly one effect label from
the row; nested effects require nested `Handle` nodes.

---

## 6. Patterns

```ebnf
branch = pattern expr  (* pattern → body *)

pattern = PWild                       (* _ *)
        | PVar   (name : type)        (* x : T  — binder carries type *)
        | PCon   name pattern*        (* constructor: Some (x : Int) *)
        | PTuple pattern+             (* tuple: (x : Int, y : Float) *)
        | PRec   { name = pattern }+  (* record: {x = (n : Int)} *)
        | PLit   literal              (* literal match: 0, "foo" *)
        | PArr   pattern* rest?       (* array: [a; b; ..tail] *)
        | PAnn   pattern type         (* typed pattern: (p : T) *)

rest = name  (* named rest binding: ..xs *)
     | '_'   (* anonymous rest: .. *)
```

All variable binders in patterns carry their types (`PVar (x : T)`).
`PWild` does not bind. `PAnn` is used to propagate type constraints from
typed surface patterns into Core.

---

## 7. Kinds

```ebnf
kind = KType           (* Type  — value-level types *)
     | KNat            (* Nat   — dimension variables *)
     | KEffect         (* Effect — effect labels *)
     | KRow            (* Row   — record / effect rows *)
     | KField          (* Field — field name tokens *)
     | KFun kind kind  (* kind arrow: K₁ → K₂ *)
```

Kind annotations appear on `TForall` binders. Kinds are inferred everywhere
else and need not appear explicitly in Core terms, but are tracked by the
type checker for each phase.

---

## 8. Primitives

Primitives are opaque named operations. The treewalk interprets them directly;
the compiler emits corresponding C or inline assembly. All primitives are fully
typed; the type checker validates argument and result types at the `Prim` node.

Primitives divide into three categories.

### 8.1 Irreducible Primitives

Fixed bedrock operations. This set is closed — new entries require a compiler
change. All backends must implement every irreducible primitive.

**Arithmetic:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.add_int` | `Int, Int → Int` | |
| `prim.sub_int` | `Int, Int → Int` | |
| `prim.mul_int` | `Int, Int → Int` | |
| `prim.div_int` | `Int, Int → Int` | truncating toward zero |
| `prim.mod_int` | `Int, Int → Int` | remainder; sign follows dividend |
| `prim.neg_int` | `Int → Int` | |
| `prim.add_float` | `Float, Float → Float` | |
| `prim.sub_float` | `Float, Float → Float` | |
| `prim.mul_float` | `Float, Float → Float` | |
| `prim.div_float` | `Float, Float → Float` | |
| `prim.neg_float` | `Float → Float` | |

**Comparisons:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.eq_int` | `Int, Int → Bool` | |
| `prim.lt_int` | `Int, Int → Bool` | |
| `prim.le_int` | `Int, Int → Bool` | |
| `prim.eq_float` | `Float, Float → Bool` | |
| `prim.lt_float` | `Float, Float → Bool` | |
| `prim.le_float` | `Float, Float → Bool` | |

**Bitwise (Int):**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.and_int` | `Int, Int → Int` | |
| `prim.or_int` | `Int, Int → Int` | |
| `prim.xor_int` | `Int, Int → Int` | |
| `prim.not_int` | `Int → Int` | bitwise complement |
| `prim.shl_int` | `Int, Int → Int` | shift left |
| `prim.shr_int` | `Int, Int → Int` | logical shift right |

**Coercions:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.int_to_float` | `Int → Float` | zero-cost |
| `prim.float_to_int` | `Float → Int` | truncating toward zero |
| `prim.unsafe_coerce` | `∀ a b ⇒ a → b` | zero-cost; used for `distinct` wrapping |

**Memory and arrays:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.vec_get` | `∀ n a ⇒ a[n], Nat → a` | bounds-checked in treewalk |
| `prim.vec_set` | `∀ n a ⇒ a[n], Nat, a → a[n]` | |
| `prim.vec_len` | `∀ n a ⇒ a[n] → Nat` | |
| `prim.vec_alloc` | `∀ n a ⇒ Nat → a[n]` | uninitialized |
| `prim.vec_copy` | `∀ n a ⇒ a[n] → a[n]` | full copy |

**Records and tuples:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.rec_get` | `∀ f T rest ⇒ {f: T \| rest}, Field → T` | |
| `prim.rec_set` | `∀ f T rest ⇒ {f: T \| rest}, Field, T → {f: T \| rest}` | |
| `prim.tuple_get` | `∀ n a ⇒ (… a …), Nat → a` | 1-indexed |

**Assertions:**

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.assert_dim` | `Bool → ()` | Z3-validated bounds assertion |
| `prim.assert_bounds` | `Nat, Nat → ()` | runtime index bounds check |
| `prim.panic` | `∀ a ⇒ String → a` | unconditional abort |

Target-specific primitives (SIMD intrinsics, memory barriers, etc.) may be added
by backends; they do not appear in the treewalk interpreter.

### 8.2 SOAC Primitive Markers

SOAC primitives are **not irreducible** — they are semantic markers placed in
Core by the frontend to signal structure that the fusion passes can act on.
The treewalk interprets them as ordinary structural recursion; the compiler
recognizes them as fusion targets in Layer 1 and Layer 2 (see [BACKEND.md §2.3](BACKEND.md#23-layer-1--algebraic-rewriting),
[§2.4](BACKEND.md#24-layer-2--soac-fusion)).

User code that avoids these forms — writing raw `prim.vec_get`/`prim.vec_set`
loops — is valid but **fusion-opaque**: the compiler cannot fuse it.

| Primitive | Signature | Notes |
|-----------|-----------|-------|
| `prim.vec_map` | `∀ n a b ⇒ a[n], (a → b) → b[n]` | Layer 1/2 fusion target |
| `prim.vec_filter` | `∀ n a ⇒ a[n], (a → Bool) → ∃(m ≤ n) a[m]` | streaming in fused chains |
| `prim.vec_fold` | `∀ n a b ⇒ a[n], b, (b, a → b) → b` | terminates a vertical chain |
| `prim.vec_scan` | `∀ n a b ⇒ a[n], b, (b, a → b) → b[n]` | prefix reduction |
| `prim.vec_zip` | `∀ n a b ⇒ a[n], b[n] → (a, b)[n]` | paired iteration |
| `prim.vec_flatMap` | `∀ n a b m ⇒ a[n], (a → b[m]) → b[n*m]` | dimension tracked |
| `prim.vec_concat` | `∀ n m a ⇒ a[n], a[m] → a[n+m]` | |

### 8.3 FFI — Extern Declarations

`extern "C"` binds a C symbol by name. The `Extern` node in Core carries the
symbol name, an optional C expression body (for macro wrapping), and the
declared Arra type.

```ebnf
(* Core term — extends the expr grammar in §2 *)
expr = ...
     | Extern  string (string option) type
     (*          ^           ^           ^
              symbol     expr body   declared type *)
```

`Extern` nodes are distinct from `Prim` nodes: the primitive set is
compiler-internal; `Extern` symbols are user-supplied.

**No overhead concern.** Arra's backend emits C. At each `Extern` site the
emitter produces a C call or, when an expression body is present, injects the
C expression directly. Since the output is C, the C compiler sees both the
Arra-generated code and the FFI implementation simultaneously and applies its
full optimization budget — inlining, constant folding, CSE — across the
boundary. Inlining is the C compiler's domain, not Arra's. The
`/'-Header "..."-'/` attribute ([SYNTAX.md §9.10](SYNTAX.md#910-extern-declarations)) tells the emitter which
`#include` to inject so that `static inline` definitions are visible to the C
compiler at the call site.

**Effect:** every `Extern` call has effect `⟪IO⟫` unless the declaration
carries `/'-Pure-'/`, in which case the effect row is empty. This is a
*type-system* distinction — it governs Arra's effect tracking and hook dispatch.
It is not a performance annotation; inlining is independent of it. The type
checker enforces it: an `Extern` appearing in a pure context requires
`/'-Pure-'/`.

**Fusion:** `Extern` nodes are always fusion-opaque to Arra's own passes. The
compiler never moves, reorders, or eliminates an `Extern` call based on SOAC
rewriting. `/'-Pure-'/` relaxes the effect constraint only. After C emission,
the C compiler may apply its own optimizations across `Extern` call sites
freely — Arra makes no attempt to prevent or direct this.

**Expression body:** when an `Extern` carries a C expression string, the
emitter injects the expression directly at the call site rather than emitting a
function call. This is the mechanism for wrapping C macros that are not
callable as functions. Argument binding into the expression string is an open
question ([semantics/09_FFI.md §9.5](semantics/09_FFI.md#95-expression-body-externs)).

**`Ptr` type:** C pointer arguments and return values use `Ptr a` — an opaque
pointer type not subject to GC. `Ptr Byte` is the canonical `void*` analogue.
See [semantics/09_FFI.md §9](semantics/09_FFI.md#9-ffi--foreign-function-interface) for type mapping and safety rules.

---

## 9. Pretty Printer

Core dumps are the primary debugging surface. The pretty printer accepts flags
to suppress noise for specific contexts:

| Flag | Effect |
|------|--------|
| `--no-types` | Suppress type annotations on binders and `Ann` nodes |
| `--no-dims` | Suppress dimension expressions in `TArr` — print `a[]` |
| `--no-kinds` | Suppress kind annotations on `TForall` |
| `--no-effects` | Suppress effect rows in function types |
| `--no-prims` | Abbreviate `Prim` nodes to just the primitive name |

Default (no flags): print everything. Flags combine freely.

---

## 10. Lowering Summary

How surface constructs and hook forms desugar to Core:

| Surface | Core |
|---------|------|
| `r.x` | `ProjF r x` |
| `r.(x ← v)` | `Prim prim.rec_set [r; Field.x; v]` |
| `t.1` | `ProjI t 1` |
| `(a, b, c) f` | `App f (Tuple [a; b; c])` |
| `iso A B (…)` | Two `Let` bindings for forward / backward `from` |
| `p @ name` | `Let (name : T) scrutinee (Case scrutinee T [p → body])` |
| `(f → p)` | `Case (App f scrutinee) T [p → body]` |
| `x-` | `Prim prim.neg_? [x]` (uop; resolved by type of `x`) |
| `bop + x y` | `Prim prim.add_? [x; y]` (resolved by type) |
| `iter 'map` | `Prim prim.vec_map [arr; f]` (after hook resolution) |
| `[head; ..tail]` | `Case arr T [PArr [head] tail → body]` |
| `{x: 0, ..}` | `Case rec T [PRec [{x = PLit 0}] → body]` |
| `handle e with h` | `Handle e h` |
| `perform op x` | `Perform op [x]` |
| `ctl op x → body` | `HCtl op (x : T) body` in enclosing `Handle` |
| `distinct Foo ← T` | Newtype; `Prim prim.unsafe_coerce` at wrap/unwrap sites |

---

## 11. Open Questions

Backend-related questions (effect lowering, coercion nodes, Loop IR) are tracked
in [BACKEND.md §8](BACKEND.md#8-open-questions).
