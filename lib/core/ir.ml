(** Core IR — the split point between the frontend and both backends.

    Invariants that hold by the time any code reaches Core:
    - All hooks resolved to specific implementations
    - All surface sugar eliminated (field access, record update, iso, derived, tuple index)
    - All macros (Program T splices) expanded
    - Types explicit on every binder
    - Dimensions explicit; Z3 has verified all constraints
    - Effects explicit; Perform/Handle appear wherever effects are introduced/discharged
    - Modules erased; all names fully qualified

    Core is fully monomorphic after the specialization pass (no TVar or TForall remain).
    See doc/CORE.md for the full specification. *)

(** ─── Identifiers ───────────────────────────────────────────────────────── *)

type name = string

type id = { name : name; stamp : int }

let fresh_stamp =
  let n = ref 0 in
  fun () -> incr n; !n

let fresh name = { name; stamp = fresh_stamp () }

(** ─── Kinds ─────────────────────────────────────────────────────────────── *)

(** See doc/CORE.md §7. Kinds classify type-level variables. *)
type kind =
  | KType             (** Type  — value-level types *)
  | KNat              (** Nat   — dimension variables *)
  | KEffect           (** Effect — effect labels *)
  | KRow              (** Row   — record / effect rows *)
  | KField            (** Field — field name tokens *)
  | KFun of kind * kind  (** kind arrow K₁ → K₂ *)

(** ─── Dimension expressions ─────────────────────────────────────────────── *)

(** Quantifier-free linear arithmetic over Nat.
    Z3 has verified all constraints before Core is produced.
    See doc/CORE.md §4. *)
type dim =
  | DVar of name
  | DLit of int
  | DAdd of dim * dim
  | DSub of dim * dim
  | DMul of dim * dim
  | DDiv of dim * dim      (** integer division *)

(** ─── The big mutually recursive block ──────────────────────────────────── *)
(** [ty], [eff], [expr], [pat] are mutually recursive because TSigma
    embeds an [expr] constraint inside a [ty], and [expr] uses [ty] for
    annotations and [pat] for case branches. *)

(** Effect rows.  See doc/CORE.md §5. *)
type eff =
  | EffEmpty                          (** ⟪⟫ — pure *)
  | EffVar   of name                  (** effect row variable: e *)
  | EffCon   of name * ty list * eff  (** labelled effect: Fail T | rest *)

(** Core type language.  See doc/CORE.md §3.
    Post-specialization: no TVar or TForall remain. *)
and ty =
  | TVar    of name                   (** type variable: a, b *)
  | TCon    of name                   (** type constructor: Int, Bool, Float, … *)
  | TApp    of ty * ty                (** type application *)
  | TFun    of ty * ty                (** pure function: a → b *)
  | TEffFun of ty * eff * ty          (** effectful function: a → ⟪e⟫ b *)
  | TTuple  of ty list                (** tuple type: (A, B, C) *)
  | TRecord of (name * ty) list       (** record type: {x: Int, y: Float} — NOT normalized to tuple *)
  | TArr    of ty * dim               (** array: a[n] — dim carries shape info through all passes *)
  | TForall of name * kind * ty       (** ∀ a : K ⇒ T — eliminated by specialization pass *)
  | TSigma  of name * ty * expr * ty  (** ∃(x : T, P) U — bounded existential *)
  | TEffect of eff                    (** effect row as a type *)
  | TNever                            (** ⊥ — bottom type *)

(** Literals *)
and lit =
  | LInt   of int
  | LFloat of float
  | LBool  of bool
  | LByte  of int
  | LUnit

(** Core patterns.  See doc/CORE.md §6.
    All variable binders carry their types. *)
and pat =
  | PWild                               (** _ *)
  | PVar   of id * ty                   (** x : T — binder with type *)
  | PCon   of name * pat list           (** constructor with sub-patterns *)
  | PTuple of pat list                  (** (x : Int, y : Float) *)
  | PRec   of (name * pat) list         (** {x = (n : Int), …} *)
  | PLit   of lit                       (** literal match *)
  | PArr   of pat list * id option      (** [a; b; ..rest] *)
  | PAnn   of pat * ty                  (** typed pattern: (p : T) *)

(** A case branch: pattern → body. *)
and branch =
  { br_pat  : pat
  ; br_body : expr
  }

(** Effect handler clauses.  See doc/CORE.md §5.
    - HCtl: non-resumable; does NOT receive continuation
    - HOp:  resumable; receives k : result → ⟪rest⟫ a
    - HReturn: transforms the return value of the handled computation *)
and handler_clause =
  | HCtl    of name * (id * ty) * expr
  (** ctl opName (x : T) → body *)
  | HOp     of name * (id * ty) * (id * ty) * expr
  (** op opName (x : T) (k : T → ⟪rest⟫ a) → body *)
  | HReturn of (id * ty) * expr
  (** return (x : T) → body *)

(** Core expressions.  See doc/CORE.md §2. *)
and expr =
  | Lit     of lit
  | Var     of id

  (** Lambda — binders carry types; multi-param for practical convenience *)
  | Lam     of (id * ty) list * expr

  (** Type abstraction: Λa : K. e — eliminated by specialization pass *)
  | TyLam   of name * kind * expr

  (** Application — single argument; multi-arg = nested App *)
  | App     of expr * expr list

  (** Type application: e [T] — eliminated by specialization pass *)
  | TyApp   of expr * ty

  (** Non-recursive let — binder carries type *)
  | Let     of id * ty * expr * expr

  (** Mutually recursive group — each binder carries type *)
  | LetRec  of (id * ty * expr) list * expr

  (** Case — carries explicit return type so passes need not re-infer *)
  | Case    of expr * ty * branch list

  (** Constructor application *)
  | Con     of name * expr list

  (** Tuple construction *)
  | Tuple   of expr list

  (** Named record construction *)
  | Record  of (name * expr) list

  (** Array literal with dimension annotation *)
  | Array   of expr list * dim

  (** Tuple field access, 1-indexed: e.1, e.2 *)
  | ProjI   of expr * int

  (** Record field access: e.x *)
  | ProjF   of expr * name

  (** Effect operation: raise *)
  | Perform of name * expr list

  (** Effect handler installation — discharges exactly one effect label *)
  | Handle  of expr * handler_clause list

  (** Primitive operation — all argument types validated by type checker *)
  | Prim    of prim * expr list

  (** FFI extern call or expression-body injection *)
  | Extern  of extern_ref * expr list

  (** Type annotation — survives all Core passes; backends may erase at emission *)
  | Ann     of expr * ty

(** Primitive operations.  See doc/CORE.md §8.
    Irreducible prims: all backends must implement every one.
    SOAC markers: semantic markers for fusion; treewalk interprets as structural recursion. *)
and prim =
  (** ── Arithmetic: Int ─────────────────────────────────────────────────── *)
  | AddInt | SubInt | MulInt | DivInt | ModInt | NegInt
  (** ── Arithmetic: Float ───────────────────────────────────────────────── *)
  | AddFloat | SubFloat | MulFloat | DivFloat | NegFloat
  (** ── Bitwise: Int ────────────────────────────────────────────────────── *)
  | AndInt | OrInt | XorInt | NotInt | ShlInt | ShrInt
  (** ── Comparison: Int ─────────────────────────────────────────────────── *)
  | EqInt | LtInt | LeInt | GtInt | GeInt
  (** ── Comparison: Float ───────────────────────────────────────────────── *)
  | EqFloat | LtFloat | LeFloat
  (** ── Boolean ────────────────────────────────────────────────────────── *)
  | BNot | BAnd | BOr | BEq
  (** ── Numeric coercions ───────────────────────────────────────────────── *)
  | IntToFloat    (** Int → Float — zero-cost *)
  | FloatToInt    (** Float → Int — truncating toward zero *)
  | UnsafeCoerce  (** ∀ a b ⇒ a → b — zero-cost; distinct wrapping; eliminated pre-emission *)
  (** ── Arrays ─────────────────────────────────────────────────────────── *)
  | VecGet    (** a[n], Nat → a — bounds-checked in treewalk *)
  | VecSet    (** a[n], Nat, a → a[n] *)
  | VecLen    (** a[n] → Nat *)
  | VecAlloc  (** Nat → a[n] — uninitialized *)
  | VecCopy   (** a[n] → a[n] — full copy *)
  (** ── Records and tuples ─────────────────────────────────────────────── *)
  | RecGet    (** {f: T | rest}, Field → T *)
  | RecSet    (** {f: T | rest}, Field, T → {f: T | rest} *)
  | TupleGet  (** (… a …), Nat → a — 1-indexed *)
  (** ── Assertions and control ─────────────────────────────────────────── *)
  | AssertDim     (** Bool → () — Z3-validated shape assertion; live in treewalk *)
  | AssertBounds  (** Nat, Nat → () — runtime index bounds check *)
  | Panic         (** String → a — unconditional abort *)
  (** ── SOAC fusion markers ────────────────────────────────────────────── *)
  (** Placed in Core by the frontend to signal structure for fusion passes.
      Treewalk interprets as ordinary structural recursion.
      Compiler recognises as Layer 1 / Layer 2 fusion targets.
      See doc/BACKEND.md §2 for the fusion taxonomy. *)
  | VecMap       (** a[n], (a → b) → b[n] *)
  | VecFilter    (** a[n], (a → Bool) → ∃(m ≤ n) a[m]  — streaming in fused chains *)
  | VecFold      (** a[n], (b, a → b), b → b  — terminates vertical chain *)
  | VecScan      (** a[n], (b, a → b), b → b[n]  — prefix reduction *)
  | VecZip       (** a[n], b[n] → (a, b)[n] *)
  | VecMapMaybe  (** a[n], (a → Option b) → ∃(m ≤ n) b[m] *)
  | VecFlatMap   (** a[n], (a → b[m]) → b[n*m] *)
  | VecTraverse  (** a[n], (a → ⟪Nondet⟫ b) → ⟪Nondet⟫ b[n] *)
  | VecConcat    (** a[n], a[m] → a[n+m] *)

(** FFI extern reference — call-form or expression-body.
    See doc/semantics/09_FFI.md and doc/CORE.md §8.3. *)
and extern_ref =
  { er_symbol : string        (** C symbol name — must match ABI exactly *)
  ; er_body   : string option (** C expression string for macro wrapping; None = call form *)
  ; er_pure   : bool          (** /'-Pure-'/ — relaxes effect constraint only *)
  ; er_header : string option (** /'-Header "…"-'/ — injected #include *)
  }

(** ─── Top-level definitions ─────────────────────────────────────────────── *)

type def =
  { id   : id
  ; ty   : ty
  ; expr : expr
  }

type program = { defs : def list; entry : id option }
