(** Surface AST — produced by the parser, consumed by the type checker.

    Represents the full surface syntax of Arra before any desugaring.
    See doc/SYNTAX.md for the grammar this reflects. *)

(* Bring sexp_of_string, sexp_of_int, sexp_of_list, etc. into scope so that
   [@@deriving sexp_of] works without a Core / Base dependency. *)
open Sexplib.Std

(* Shadow Z to add sexp support — type identity is preserved via include. *)
module Z = struct
  include Z
  let sexp_of_t z = Sexplib.Sexp.Atom (to_string z)
end

(** ─── Basic tokens ──────────────────────────────────────────────────────── *)

(* Locations are suppressed in sexp output — the parser fills them with
   dummy values anyway, so they add noise without information. *)
type loc = { file : string; line : int; col : int }
let sexp_of_loc _ = Sexplib.Sexp.Atom ""

type name = string [@@deriving sexp_of]  (** lowercase identifier *)
type ctor = string [@@deriving sexp_of]  (** uppercase identifier / constructor *)
type tick = string [@@deriving sexp_of]  (** tick name: 'map, 'fold, 'rank, … *)
type op   = string [@@deriving sexp_of]  (** operator symbol: +, *, ÷, … *)

(** ─── Literals ──────────────────────────────────────────────────────────── *)

type lit =
  | LInt    of Z.t          (** integer: 42, 0xFF, 0b1010, 1_000_000 *)
  | LFloat  of float        (** float: 3.14, 1.5e-3 *)
  | LBool   of bool         (** true / false *)
  | LStr    of string       (** "hello" or """multiline""" *)
  | LSymbol of string       (** `bid — runtime-interned symbol *)
  | LUnit                   (** () *)
  | LField  of string       (** .x — first-class field name *)
[@@deriving sexp_of]

(** ─── Dimension expressions (LIA) ──────────────────────────────────────── *)

(** Quantifier-free linear arithmetic over Nat.
    Checked by the Z3 backend; see semantics/02_TYPES.md §2.5.
    Nonlinear terms (n*m) fall back to a[] with a runtime assertion. *)
type dim =
  | DLit  of int
  | DVar  of name
  | DAdd  of dim * dim
  | DSub  of dim * dim
  | DMul  of int * dim          (** scalar × dim — linear only *)
  | DMulD of dim * dim          (** dim × dim — non-linear, best-effort Z3 *)
[@@deriving sexp_of]

(** ─── Kind system ───────────────────────────────────────────────────────── *)

type kind =
  | KType   (** value types *)
  | KNat    (** natural number indices / array dimensions *)
  | KEffect (** effect rows *)
  | KRow    (** record row tails *)
  | KField  (** record field names *)
[@@deriving sexp_of]

(** In kind-annotation position a name may be a kind or a trait constraint. *)
type kind_or_trait =
  | KKind  of kind
  | KTrait of name              (** e.g. (a : Functor) *)
[@@deriving sexp_of]

(** A universally-quantified type variable, optionally annotated. *)
type tvar =
  | TVFree of name              (** kind inferred from usage *)
  | TVAnno of name * kind_or_trait  (** explicit: (a : Nat), (a : Functor) *)
[@@deriving sexp_of]

(** ─── Attributes ────────────────────────────────────────────────────────── *)

(** A single attribute entry inside /'-…-'/. *)
type attr_entry =
  | AEFlag  of ctor             (** /'-Pure-'/ *)
  | AEStr   of ctor * string    (** /'-Header "<math.h>"-'/ *)
  | AEName  of ctor * name      (** /'-Delta f-'/ *)
  | AEInt   of ctor * int       (** /'-MapTest samples:200-'/ *)
[@@deriving sexp_of]

(** An attribute block: one or more semicolon-separated entries. *)
type attr = attr_entry list [@@deriving sexp_of]

(** ─── Module paths and import filters ───────────────────────────────────── *)

(** Qualified module path: Math, Math.Vector, etc. *)
type module_path = ctor list [@@deriving sexp_of]

type import_item =
  | IIName  of name             (** value / function name *)
  | IIOp    of op               (** operator: (÷), (+) *)
  | IITick  of tick             (** modifier: 'map, 'fold *)
  | IIType  of ctor             (** type or module name *)
[@@deriving sexp_of]

type open_filter =
  | OFOnly   of import_item list  (** open M (f, (+), 'map) *)
  | OFHiding of import_item list  (** open M hiding (f) *)
[@@deriving sexp_of]

(** ─── Hook system ───────────────────────────────────────────────────────── *)

type hook_kw =
  | HBop | HUop | HApp | HPat | HProject | HAssign | HView
  | HIter | HGen | HFrom
[@@deriving sexp_of]

type hook_sym =
  | HSOp   of op               (** operator hook: bop +, uop - *)
  | HSTick of tick             (** tick hook: iter 'map, gen 'generate *)
[@@deriving sexp_of]

(** ─── The big mutually recursive block ──────────────────────────────────── *)
(** Everything from here to [decl] is mutually recursive because:
    - [ty] references [expr] (sigma constraint) and [eff_entry] (effect rows)
    - [expr] references [ty], [pat], [branch], [handler_clause], [stmt]
    - [pat] references [expr] (view patterns) and [ty] (typed patterns)
    The [decl] type is defined after and references all of the above. *)

type eff_entry =
  | EENamed of ctor * ty list   (** named effect with ≥0 type args: IO, State DB, Fail E *)
  | EEVar   of name             (** effect row variable: e, fx *)

(** An effect row: ⟪IO | State DB | e⟫.
    EPure = ⟪⟫; ERow with only an EEVar = just the variable. *)
and eff =
  | EPure                       (** ⟪⟫ — no effects *)
  | ERow  of eff_entry list     (** ≥1 entries *)

(** ─── Types ─────────────────────────────────────────────────────────────── *)

and ty =
  | TName   of name             (** named type: Int, Float, Bool, … *)
  | TVar    of name             (** type variable: a, b, t *)
  | TApp    of ty * ty          (** type application by juxtaposition *)
  | TArr    of ty * dim list    (** a[n;m;…] — array type with dimensions *)
  | TDyn    of ty               (** a[] — dynamic-size array (escape hatch) *)
  | TFun    of ty * eff option * ty
  (** a →⟨e?⟩ b — single arrow; chain via nesting for curried types *)
  | TUnit                       (** () — unit type *)
  | TNever                      (** ⊥ / Never — bottom type *)
  | TProduct of ty list         (** (A, B, C) — product/tuple type, ≥2 elements *)
  | TRecord  of record_ty       (** {x: T, y: U} or open {x: T | rest} *)
  | TSigma   of name * ty * expr * ty
  (** ∃(m : kind_ty, constraint_expr) result_ty
      binder name · binder type · constraint expression · result type *)
  | TConstrained of tvar list * ty  (** ∀ a b ⇒ ty — constrained polymorphism *)
  | TRowConcat of ty * ty       (** {A} ++ {B} — disjoint row concatenation *)
  | TRowSub    of ty * name     (** {A} - .x — field removal *)

(** Closed or open record type. *)
and record_ty =
  | RClosed of field_ty list
  | ROpen   of field_ty list * name   (** {x: T | rest} — open with row tail variable *)

and field_ty = { fname : name; fty : ty }

(** ─── Patterns ──────────────────────────────────────────────────────────── *)

and pat =
  | PWild                                   (** _ *)
  | PVar    of loc * name                   (** x *)
  | PCtor   of loc * ctor * pat list        (** Some x, None, Point x y *)
  | PLit    of loc * lit                    (** 0, 3.14, true *)
  | POr     of loc * pat list               (** (A | B | C) — ≥2 alternatives *)
  | PTuple  of loc * pat list               (** (x, y, z) — tuple / sigma elimination *)
  | PTyped  of loc * pat * ty               (** (x : T) — typed pattern *)
  | PArray  of loc * list_pat               (** [a; b; ..rest] *)
  | PRecord of loc * record_pat             (** {x, y: p, ..rest} *)
  | PView   of loc * expr * pat             (** (f → p) — view pattern *)
  | PShape  of loc * pat * dim list         (** x[n;m] — shape annotation suffix *)
  | PAs     of loc * pat * name             (** p @ x — as-pattern (bind whole match) *)

(** Array/list pattern: prefix elements, optional rest, optional suffix. *)
and list_pat =
  { lp_prefix : pat list
  ; lp_rest   : name option         (** ..rest; None = .._ (unnamed) *)
  ; lp_suffix : pat list            (** elements after the rest *)
  }

(** Record pattern: named fields + optional open tail. *)
and record_pat =
  { rp_fields : (name * pat option) list  (** {x} or {x: p} *)
  ; rp_open   : name option               (** None = closed; Some "" = ..; Some "r" = ..r *)
  }

(** ─── Expressions ───────────────────────────────────────────────────────── *)

and expr =
  (** Atoms *)
  | Lit         of loc * lit
  | Var         of loc * name
  | Ctor        of loc * ctor             (** uppercase constructor as value *)
  | FieldName   of loc * name             (** .x — first-class field name *)
  | TickVal     of loc * tick             (** 'map as a first-class value *)

  (** Application — Forth-style: App(f, g) means g(f); `f g` parses to App(f, g) *)
  | App         of loc * expr * expr

  (** Tick application: arr 'map f, arr 'rank 1 f
      (tick, optional extra value arg, optional projection arg) *)
  | TickApp     of loc * expr * tick * expr option * expr option

  (** Operator application *)
  | Bop         of loc * op * expr * expr   (** a + b — infix binary *)
  | Uop         of loc * op * expr          (** x- — postfix unary *)

  (** Sections: (op), (1+), (+1), ('map f), ('rank 1 f) *)
  | Section     of loc * section_form

  (** Lambda / dispatch — single branch = lambda; multiple = pattern dispatch *)
  | Lam         of loc * branch
  | Branches    of loc * branch list

  (** Recursive binding: rec x ← e or rec (x ← e; y ← e) — mutual *)
  | Rec         of loc * rec_binding list * expr

  (** Type annotation *)
  | Annot       of loc * expr * ty

  (** Tuples: (a, b, c) — ≥2 elements *)
  | Tuple       of loc * expr list

  (** Array literal: [1; 2; 3] *)
  | Array       of loc * expr list

  (** Range: lo..hi or lo..step..hi — produces Range Int *)
  | Range       of loc * expr * expr * expr option  (** lo, hi, optional step *)

  (** Record literal: {x = 1, y = 2} *)
  | Record      of loc * (name * expr) list

  (** Tuple index: t.1 — 1-indexed *)
  | TupleIdx    of loc * expr * int

  (** Field update: r.(x ← v; y ← w) *)
  | FieldUpdate of loc * expr * (name * expr) list

  (** Quoted program: `[expr] — produces Program T *)
  | Quoted      of loc * expr

  (** Unquoted splice: .(expr) — splices Program T into T-typed position *)
  | Splice      of loc * expr

  (** Sequential / do-notation block *)
  | Do          of loc * stmt list

(** ─── Section forms ─────────────────────────────────────────────────────── *)

and section_form =
  | SBareOp      of op                     (** (+) *)
  | SLeftOp      of expr * op              (** (1+) *)
  | SRightOp     of op * expr              (** (+1) *)
  | SBareTick    of tick                   (** ('map) *)
  | SLeftTick    of expr * tick            (** (arr 'map) *)
  | SRightTick   of tick * expr            (** ('map f) *)
  | STickVal     of tick * expr * expr     (** ('rank 1 f) — tick + extra val + proj_arg *)
  | SHandle      of handler_clause list    (** ('handle (clauses…)) *)

(** ─── Branches ──────────────────────────────────────────────────────────── *)

and branch =
  { b_pat   : pat
  ; b_guard : expr option           (** when expr — optional guard *)
  ; b_body  : expr
  }

(** ─── Effect handler clauses ────────────────────────────────────────────── *)

and handler_clause =
  | HCtl    of name * pat list * expr           (** ctl opName pats → body *)
  | HOp     of name * pat list * name * expr    (** op opName pats k → body; k = continuation *)
  | HReturn of pat * expr                       (** return pat → body *)

(** ─── Sequential statements (do-notation) ──────────────────────────────── *)

and stmt =
  | SExpr    of expr                   (** bare expression *)
  | SBind    of pat * expr             (** pat ← expr — pure or effectful *)
  | STypeSig of name * ty              (** type annotation within a do-block *)
  | SRec     of rec_binding list       (** rec group within a do-block *)
  | SOpen    of module_path * open_filter option  (** open M — scopes to rest of block *)

(** ─── Recursive bindings ─────────────────────────────────────────────────── *)

and rec_binding =
  { rb_pat  : pat
  ; rb_ty   : ty option
  ; rb_expr : expr
  }

(** ─── Top-level declarations ────────────────────────────────────────────── *)

(** The head of a hook definition — varies by keyword.
    See doc/SYNTAX.md §9.2 for the full grammar. *)
and hook_head =
  | HHSym of hook_sym * name option * pat list * ty option
  (** symbolic / tick: sym, optional effect-var [e], patterns, optional result type
      used by: bop, uop, iter, gen, app *)
  | HHPat of ctor * pat list * ty option
  (** pat hook: pat Ctor patterns, optional result type *)
  | HHPos of name option * pat list * ty option
  (** positional: optional effect-var [e], patterns, optional result type
      used by: project, assign, view, from *)

and decl =
  (** Value binding: pat ← expr, with optional preceding type signature *)
  | DVal      of loc * attr list * pat * ty option * expr

  (** Recursive binding block: rec (x ← e; y ← e) *)
  | DRec      of loc * attr list * rec_binding list

  (** Hook definition: bop + Int, Int → Int ← prim.add_int *)
  | DHook     of loc * attr list * tvar list * hook_kw * hook_head * expr

  (** Isomorphism: iso T, U (from p1 ← e1; from p2 ← e2) *)
  | DIso      of loc * attr list * tvar list * pat * pat
                 * (pat * expr)   (** first  from direction *)
                 * (pat * expr)   (** second from direction *)

  (** Algebraic data type: data Point ← {x: Int, y: Int} | None | Some a *)
  | DData     of loc * attr list * ctor * tvar list * data_body option

  (** Type synonym: type String ← Char[] *)
  | DTypeSyn  of loc * ctor * tvar list * ty

  (** Distinct/newtype: distinct Celsius ← Float *)
  | DDistinct of loc * ctor * tvar list * ty

  (** Effect definition: effect Fail e (ctl raise : e → ⊥) *)
  | DEffect   of loc * ctor * name list * effect_op list

  (** Trait definition: [∀ supers ⇒] trait Name params (sigs) *)
  | DTrait    of loc * tvar list * ctor * name list * trait_sig list

  (** Implementation block: [∀ constraints ⇒] implementation Trait Type+ (methods) *)
  | DImpl     of loc * tvar list * ctor * ty list * decl list

  (** Module definition: module Name (interface (…) implementation (…)) *)
  | DModule   of loc * ctor * decl list option * decl list option

  (** Open statement: open Math or open Math (f, g) hiding (h) *)
  | DOpen     of loc * module_path * open_filter option

  (** Extern C declaration: extern "C" malloc : Nat → Ptr Byte *)
  | DExtern   of loc * attr list * name * ty * extern_body option

  (** Derived view: derived activeTrades ← expr *)
  | DDerived  of loc * name * ty option * expr

  (** Attribute scope block: /'-Attr-'/ applied to a group of decls *)
  | DAttrScope of loc * attr * decl list

(** ─── Data body ─────────────────────────────────────────────────────────── *)

and data_body =
  | DBRecord of field_ty list      (** {x: Int, y: Float} *)
  | DBEnum   of variant list       (** None | Some a *)

and variant = ctor * ty list       (** constructor name + field types *)

(** ─── Effect operations ──────────────────────────────────────────────────── *)

and effect_op =
  | EOCtl of name * ty    (** ctl raise : e → ⊥ — non-resumable; result must be ⊥ *)
  | EOOp  of name * ty    (** op  yield : a → () — resumable *)

(** ─── Trait signatures ───────────────────────────────────────────────────── *)

(** A method signature in a trait body.
    The result type (if any) is embedded in [ts_head] via its [ty option] field. *)
and trait_sig =
  { ts_attrs   : attr list
  ; ts_kw      : hook_kw
  ; ts_head    : hook_head
  ; ts_default : expr option    (** default implementation, if any *)
  }

(** ─── Extern declarations ───────────────────────────────────────────────── *)

and extern_body =
  | EBSymbol of string                        (** symbol rename: alternate C symbol name *)
  | EBExpr   of (name * ty) list * string     (** expression body: params + inline C expression string *)
[@@deriving sexp_of]

(** ─── Compilation unit ──────────────────────────────────────────────────── *)

type program = { decls : decl list } [@@deriving sexp_of]
