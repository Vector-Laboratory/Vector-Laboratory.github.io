%{
(** Menhir grammar for Arra surface syntax.

    Produces a [Surface.Ast.program] from the token stream emitted by the
    layout preprocessor.  The preprocessor has already inserted [VSEMI]
    tokens, so the parser never inspects indentation.

    Key design decisions
    ────────────────────
    • All binary operators and juxtaposition have equal precedence and
      associate left-to-right — this matches Arra's LTR-equal-prec rule.
    • [ARROW] and [LARROW] are not in the [expr] grammar; they only appear
      in statement/branch/binding positions and type annotations.  This keeps
      the expression grammar free of those tokens and avoids ambiguity.
    • The LHS of a binding ([pat ← expr]) is parsed as [expr] and then
      converted to [pat] via [expr_to_pat]; same for branch heads.
    • [( )] disambiguation is done by context: the grammar uses separate
      rules for the handler-block form (starts with CTL/OP_KW/RETURN) and
      the general form.  The general form is a list of [paren_item]s; the
      builder [make_paren] decides at construction time whether the result
      is a tuple, a branch block, a lambda, a do-block, or plain grouping. *)

open Ast

(* ── Position helpers ─────────────────────────────────────────────────── *)

let loc (p : Lexing.position) : loc =
  { file = p.pos_fname; line = p.pos_lnum; col = p.pos_cnum - p.pos_bol }

(* ── Literal helpers ─────────────────────────────────────────────────── *)

(** Parse an integer literal string (handles 0x…, 0b…, 0o…, decimal,
    underscore separators). *)
let parse_int s =
  let s = String.concat "" (String.split_on_char '_' s) in
  if String.length s >= 2 && s.[0] = '0' then
    match s.[1] with
    | 'x' | 'X' -> Z.of_string ("0x" ^ String.sub s 2 (String.length s - 2))
    | 'b' | 'B' ->
      let n = String.length s - 2 in
      let v = ref Z.zero in
      for i = 2 to 2 + n - 1 do
        v := Z.add (Z.mul !v (Z.of_int 2))
               (Z.of_int (Char.code s.[i] - Char.code '0'))
      done; !v
    | 'o' | 'O' ->
      let n = String.length s - 2 in
      let v = ref Z.zero in
      for i = 2 to 2 + n - 1 do
        v := Z.add (Z.mul !v (Z.of_int 8))
               (Z.of_int (Char.code s.[i] - Char.code '0'))
      done; !v
    | _ -> Z.of_string s
  else Z.of_string s

(** Classify a raw [TLit] string into its literal kind. *)
let classify_lit s =
  let n = String.length s in
  if n = 0 then LUnit
  else match s.[0] with
  | '"' -> LStr (String.sub s 1 (n - 2))
  | '`' -> LSymbol (String.sub s 1 (n - 1))
  | _ ->
    if String.contains s '.' ||
       String.contains s 'e' || String.contains s 'E'
    then LFloat (float_of_string (String.concat "" (String.split_on_char '_' s)))
    else LInt (parse_int s)

(* ── expr → pat reinterpretation ──────────────────────────────────────── *)

let rec expr_to_pat (e : expr) : pat =
  match e with
  | Var (l, x)            -> PVar (l, x)
  | Ctor (l, c)           -> PCtor (l, c, [])
  | App (l, Ctor (_, c), arg) -> PCtor (l, c, [expr_to_pat arg])
  | App (l, base, Array (_, elems)) ->
    let to_dim = function
      | Var (_, n)       -> DVar n
      | Lit (_, LInt k)  -> DLit (Z.to_int k)
      | _                -> raise (Failure "not a valid dim in shape pattern")
    in
    PShape (l, expr_to_pat base, List.map to_dim elems)
  | Lit (l, lit)          -> PLit (l, lit)
  | Tuple (l, es)         -> PTuple (l, List.map expr_to_pat es)
  | Annot (l, e', t)      -> PTyped (l, expr_to_pat e', t)
  | Bop (l, "|", p1, p2) ->
    (match expr_to_pat p1, expr_to_pat p2 with
     | POr (_, ps1), POr (_, ps2) -> POr (l, ps1 @ ps2)
     | POr (_, ps1), p2'          -> POr (l, ps1 @ [p2'])
     | p1', POr (_, ps2)          -> POr (l, p1' :: ps2)
     | p1', p2'                   -> POr (l, [p1'; p2']))
  | FieldName (l, n)       -> PLit (l, LField n)   (* .x — literal field-name match *)
  | _                      -> raise (Failure "not a valid pattern")

(** Convert expr to pattern, raising a nice error. *)
let to_pat context e =
  try expr_to_pat e
  with Failure _ ->
    raise (Failure (Printf.sprintf "%s: expression cannot be used as pattern" context))

(* ── Paren-expr builder ──────────────────────────────────────────────── *)

(** Intermediate representation of items inside [( )]. *)
type paren_item =
  | PITypeSig of string * ty
  | PIExpr    of expr
  | PIBind    of expr * expr          (* lhs (→ pat), rhs *)
  | PIBranch  of expr * expr option * expr   (* pat-expr, guard, body *)
  | PIRec     of rec_binding list     (* rec group in a do-block *)
  | PIOpen    of module_path * open_filter option  (* open M — scopes to rest of block *)

let make_branch lhs guard body =
  { b_pat   = to_pat "branch pattern" lhs
  ; b_guard = guard
  ; b_body  = body }

let make_do items =
  let to_stmt = function
    | PITypeSig (n, t)   -> STypeSig (n, t)
    | PIExpr e           -> SExpr e
    | PIBind (lhs, rhs)  -> SBind (to_pat "do-bind pattern" lhs, rhs)
    | PIRec bs           -> SRec bs
    | PIOpen (path, flt) -> SOpen (path, flt)
    | PIBranch _         -> failwith "branch in do-block"
  in
  List.map to_stmt items

(** Build the expression for a [( content )] paren, given [items] (the
    semicolon-separated list) and whether `,` separated multiple blocks. *)
let make_paren_items l items =
  match items with
  | []               -> Lit (l, LUnit)
  | [PIExpr e]       -> e              (* plain grouping *)
  | [PITypeSig (n, t)] -> Annot (l, Var (l, n), t)  (* (x : T) → annotated expr *)
  | _ ->
    let has_branch = List.exists (function PIBranch _ -> true | _ -> false) items in
    if has_branch then
      let branches = List.map (function
        | PIBranch (p, g, b) -> make_branch p g b
        | PIExpr e ->
          (* bare expr in branch block = branch with wildcard guard for the body? *)
          (* Actually: a bare expr in a branch block is an error; use Do instead *)
          failwith "bare expression in branch block"
        | _ -> failwith "type sig / bind mixed with branches"
      ) items in
      (match branches with
       | [br] -> Lam (l, br)
       | _    -> Branches (l, branches))
    else
      Do (l, make_do items)

(** Build a tuple from multiple comma-blocks. *)
let make_tuple l blocks =
  match blocks with
  | [items] -> make_paren_items l items   (* no comma → not a tuple *)
  | _       ->
    let exprs = List.map (fun items ->
      make_paren_items l items
    ) blocks in
    Tuple (l, exprs)

(* ── Tick-application helper ─────────────────────────────────────────── *)

let make_tick_app l base tick opt_val opt_arg =
  TickApp (l, base, tick, opt_val, opt_arg)

(* ── Declaration helpers ─────────────────────────────────────────────── *)

let make_val l attrs pat ty rhs =
  DVal (l, attrs, pat, ty, rhs)

%}

(* ── Token declarations ─────────────────────────────────────────────── *)

%token <string> LOWER    (* lowercase identifier or keyword *)
%token <string> UPPER    (* uppercase identifier *)
%token <string> TICK     (* tick name without leading ': "map", "fold" *)
%token <string> OP       (* operator *)
%token <string> INT      (* raw integer literal string *)
%token <float>  FLOAT    (* float value *)
%token <string> STRING   (* decoded string content (between quotes) *)
%token <string> SYMBOL   (* symbol name without backtick *)

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token LEFF REFF          (* ⟪ ⟫ *)
%token COMMA COLON SEMI VSEMI
%token AT DOTDOT DOT
%token ARROW              (* → / -> *)
%token DARROW             (* ⇒ / => *)
%token LARROW             (* ← / <- *)
%token BACKTICK_LBRACKET  (* `[ *)
%token ATTR_OPEN ATTR_CLOSE

(* Keywords *)
%token MODULE INTERFACE IMPLEMENTATION TRAIT REC EFFECT
%token TYPE_KW ALIAS_KW DISTINCT DERIVED OPEN WHEN EXTERN ISO FROM
%token FORALL EXISTS NEVER
%token CTL OP_KW RETURN
%token BOP UOP APP PAT PROJECT ASSIGN VIEW ITER GEN

%token EOF

(* ── Precedence ──────────────────────────────────────────────────────── *)

(* Everything in expr has equal precedence and associates left.
   The %left declarations below all have the same precedence level —
   this encodes the LTR-equal-prec rule.  Menhir uses the last %left
   group seen when resolving a conflict; since they're all at the same
   level, reduce always beats shift (left-assoc → reduce on conflict). *)

%nonassoc BELOW_OP                     (* used to weaken expr reductions *)
%left  OP AT DOTDOT                    (* operators + @ + .. *)
%left  DOT                             (* field access (tightest) *)

(* ── Start symbol ───────────────────────────────────────────────────── *)

%start <Ast.program> program

%%

(* ── Separators ─────────────────────────────────────────────────────── *)

sep  : SEMI {} | VSEMI {}
seps : sep {}  | seps sep {}

seps_opt : {}  | seps {}

(* ── Program ─────────────────────────────────────────────────────────── *)

program :
  seps_opt ds = decl_list seps_opt EOF   { { decls = ds } }

decl_list :
  | (* empty *)                          { [] }
  | d = decl                             { [d] }
  | ds = decl_list seps d = decl         { ds @ [d] }

(* ── Declarations ────────────────────────────────────────────────────── *)

decl :
  (* Attribute scope block: /'-Attr-'/ ( decl* ) — applies attrs to a group *)
  | ATTR_OPEN aes = attr_entries ATTR_CLOSE LPAREN ds = decl_list RPAREN
    { DAttrScope (loc $startpos, aes, ds) }

  (* Attribute-prefixed single decl: /'-attr-'/ decl *)
  | ATTR_OPEN aes = attr_entries ATTR_CLOSE d = decl
    { match d with
      | DVal (l, _, p, t, e)    -> DVal (l, [aes], p, t, e)
      | DRec (l, _, bs)         -> DRec (l, [aes], bs)
      | DHook (l, _, tvs, k, h, e) -> DHook (l, [aes], tvs, k, h, e)
      | DData (l, _, n, tvs, b) -> DData (l, [aes], n, tvs, b)
      | DExtern (l, _, n, t, b) -> DExtern (l, [aes], n, t, b)
      | other                   -> other }

  (* Type definition — no type parameters *)
  | TYPE_KW n = UPPER body = data_body_opt
    { DData (loc $startpos, [], n, [], body) }
  (* Type definition — with type parameters *)
  | TYPE_KW n = UPPER tvs = tvar_list body = data_body_opt
    { DData (loc $startpos, [], n, tvs, body) }

  (* Alias synonym — no parameters *)
  | ALIAS_KW n = UPPER LARROW t = ty
    { DTypeSyn (loc $startpos, n, [], t) }
  (* Alias synonym — with parameters *)
  | ALIAS_KW n = UPPER tvs = tvar_list LARROW t = ty
    { DTypeSyn (loc $startpos, n, tvs, t) }

  (* Distinct / newtype — no parameters *)
  | DISTINCT n = UPPER LARROW t = ty
    { DDistinct (loc $startpos, n, [], t) }
  (* Distinct / newtype — with parameters *)
  | DISTINCT n = UPPER tvs = tvar_list LARROW t = ty
    { DDistinct (loc $startpos, n, tvs, t) }

  (* Effect definition *)
  | EFFECT n = UPPER ps = lower_list LPAREN ops = effect_ops RPAREN
    { DEffect (loc $startpos, n, ps, ops) }

  (* Module *)
  | MODULE n = UPPER body = module_body
    { let (iface, impl) = body in DModule (loc $startpos, n, iface, impl) }

  (* Open *)
  | OPEN path = module_path flt = open_filter_opt
    { DOpen (loc $startpos, path, flt) }

  (* Extern — call-form or symbol-override: extern "C" f : T  or  extern f : T *)
  | EXTERN s = STRING n = LOWER COLON t = ty body = extern_body_opt
    { let _ = s in DExtern (loc $startpos, [], n, t, body) }
  | EXTERN n = LOWER COLON t = ty body = extern_body_opt
    { DExtern (loc $startpos, [], n, t, body) }

  (* Extern — expression-body with parameter list:
       extern "C" f (p : T, …) : R ← "c_expr"
       extern f (p : T, …) : R ← "c_expr" *)
  | EXTERN s = STRING n = LOWER
    LPAREN ps = extern_params RPAREN COLON t = fun_ty
    LARROW body = STRING
    { let _ = s in DExtern (loc $startpos, [], n, t, Some (EBExpr (ps, body))) }
  | EXTERN n = LOWER
    LPAREN ps = extern_params RPAREN COLON t = fun_ty
    LARROW body = STRING
    { DExtern (loc $startpos, [], n, t, Some (EBExpr (ps, body))) }

  (* Derived *)
  | DERIVED n = LOWER ann = ann_opt LARROW e = expr
    { DDerived (loc $startpos, n, ann, e) }

  (* Recursive binding *)
  | REC bs = rec_bindings
    { DRec (loc $startpos, [], bs) }

  (* Isomorphism — ISO is not a hookable keyword per the spec; it has its
     own iso_def production separate from hook_def.
     Type vars require explicit 'forall … =>' (spec: type_constraint).
     iso_pat prevents UPPER LPAREN from greedily consuming the iso body
     paren as constructor arguments. *)
  | ISO p1 = iso_pat COMMA p2 = iso_pat
    LPAREN from1 = from_impl SEMI from2 = from_impl RPAREN
    { DIso (loc $startpos, [], [], p1, p2, from1, from2) }
  | ISO FORALL tvs = tvar_list1 DARROW p1 = iso_pat COMMA p2 = iso_pat
    LPAREN from1 = from_impl SEMI from2 = from_impl RPAREN
    { DIso (loc $startpos, [], tvs, p1, p2, from1, from2) }

  (* Trait — 'trait' keyword comes first, then optional type_constraint *)
  | TRAIT tvs = type_constraint_opt n = UPPER ps = lower_list body = trait_body
    { DTrait (loc $startpos, tvs, n, ps, body) }

  (* Implementation — type_constraint? follows 'implementation' per the spec *)
  | IMPLEMENTATION tvs = type_constraint_opt n = UPPER ts = nonempty_ty_list body = impl_body
    { DImpl (loc $startpos, tvs, n, ts, body) }

  (* Hook definitions — type_constraint? follows hook_kw per the spec;
     must come before the catch-all DVal rule *)
  | kw = hook_kw tvs = type_constraint_opt hh = hook_head LARROW e = expr
    { DHook (loc $startpos, [], tvs, kw, hh, e) }

  (* Value binding — catch-all: pat ← expr
     Typed patterns like (x : T) ← expr work via paren_expr → Annot → PTyped.
     Note: (x : T) with a bare uppercase T is intercepted as a tvar annotation
     in the LR automaton; use a compound type like T[] to avoid the ambiguity. *)
  | p = val_lhs LARROW e = expr
    { DVal (loc $startpos, [], to_pat "value binding" p, None, e) }


(* ── Attribute entries ─────────────────────────────────────────────── *)

attr_entries :
  | a = attr_entry                          { [a] }
  | as_ = attr_entries SEMI a = attr_entry  { as_ @ [a] }

attr_entry :
  | n = UPPER                               { AEFlag n }
  | n = UPPER s = STRING                    { AEStr (n, s) }
  | n = UPPER x = LOWER                     { AEName (n, x) }
  | n = UPPER i = INT                       { AEInt (n, int_of_string i) }


(* ── Hook keywords and heads ───────────────────────────────────────── *)

hook_kw :
  | BOP   { HBop }   | UOP   { HUop }   | APP     { HApp }
  | PAT   { HPat }   | PROJECT { HProject } | ASSIGN { HAssign }
  | VIEW  { HView }  | ITER  { HIter }  | GEN     { HGen }
  | FROM  { HFrom }

hook_head :
  (* Symbolic / tick form: bop + T, T → R  or  iter 'map [e] T, R → R *)
  | sym = hook_sym ep = effect_param_opt ps = pat_csv rt = result_ty_opt
    { HHSym (sym, ep, ps, rt) }

  (* Pat hook: pat Ctor T → Option R *)
  | n = UPPER ps = pat_list rt = result_ty_opt
    { HHPat (n, ps, rt) }

  (* Positional: project / assign / view / from  [e] T, R → Out *)
  | ep = effect_param ps = pat_csv rt = result_ty_opt
    { HHPos (Some ep, ps, rt) }

(** Optional result type annotation on a hook head: '→' type.
    Distinct from LARROW ('←') so there is no ambiguity with the hook body. *)
result_ty_opt :
  | (* empty *)            { None }
  | ARROW t = fun_ty       { Some t }

hook_sym :
  | o = OP    { HSOp o }
  | t = TICK  { HSTick t }

effect_param_opt :
  | (* empty *)                { None }
  | LBRACKET n = LOWER RBRACKET  { Some n }

effect_param :
  | LBRACKET n = LOWER RBRACKET  { n }

pat_csv :
  | p = pat                        { [p] }
  | ps = pat_csv COMMA p = pat     { ps @ [p] }

pat_list :
  | (* empty *)          { [] }
  | ps = pat_list p = pat_atom   { ps @ [p] }


(* ── Tvar lists ────────────────────────────────────────────────────── *)

tvar_list :
  | FORALL tvs = tvar_list1 DARROW   { tvs }
  | tvs = tvar_list1                 { tvs }

tvar_list1 :
  | v = tvar                         { [v] }
  | vs = tvar_list1 v = tvar         { vs @ [v] }

(** [type_constraint_opt] — optional [∀ tvar+ ⇒] prefix as per the spec.
    Used by hook_def, trait_def, impl_def, and iso_def.  Type and alias
    definitions use bare [tvar_list] instead, since their parameters are
    positional and do not require the [∀ … ⇒] form. *)
type_constraint_opt :
  | (* empty *)                         { [] }
  | FORALL tvs = tvar_list1 DARROW      { tvs }

tvar :
  | n = LOWER                        { TVFree n }
  | LPAREN n = LOWER COLON k = kind_or_trait RPAREN   { TVAnno (n, k) }

kind_or_trait :
  | k = kind   { KKind k }
  | n = UPPER  { KTrait n }

kind :
  | LOWER  (* "Type" "Nat" "Effect" "Row" "Field" *)
    { match $1 with
      | "Type"   -> KType
      | "Nat"    -> KNat
      | "Effect" -> KEffect
      | "Row"    -> KRow
      | "Field"  -> KField
      | s        -> failwith ("unknown kind: " ^ s) }


(* ── Data body ─────────────────────────────────────────────────────── *)

data_body_opt :
  | (* empty *)                      { None }
  | LARROW body = data_body          { Some body }

data_body :
  | LBRACE fs = field_ty_list RBRACE   { DBRecord fs }
  | vs = variant_list                  { DBEnum vs }

variant_list :
  | v = variant                                { [v] }
  | vs = variant_list OP v = variant           { vs @ [v] }
  (* The OP between variants is typically "|" *)

variant :
  | n = UPPER ts = ty_list_opt   { (n, ts) }

ty_list_opt :
  | (* empty *)                  { [] }
  | ts = ty_atom_list            { ts }

ty_atom_list :
  | t = ty_atom                  { [t] }
  | ts = ty_atom_list t = ty_atom  { ts @ [t] }


(* ── Effect definition ─────────────────────────────────────────────── *)

effect_ops :
  | (* empty *)                    { [] }
  | os = effect_op_list            { os }

effect_op_list :
  | o = effect_op                           { [o] }
  | os = effect_op_list sep o = effect_op   { os @ [o] }

effect_op :
  | CTL n = LOWER COLON t = ty   { EOCtl (n, t) }
  | OP_KW n = LOWER COLON t = ty { EOOp  (n, t) }


(* ── Recursive bindings ─────────────────────────────────────────────── *)

rec_bindings :
  | b = rec_binding              { [b] }
  | LPAREN bs = rec_binding_list RPAREN   { bs }

rec_binding_list :
  | b = rec_binding              { [b] }
  | bs = rec_binding_list sep b = rec_binding   { bs @ [b] }

rec_binding :
  | p = val_lhs ann = ann_opt LARROW e = expr
    { { rb_pat  = to_pat "rec binding" p
      ; rb_ty   = ann
      ; rb_expr = e } }


(* ── Module body ────────────────────────────────────────────────────── *)

module_body :
  (* Full form: interface (…) implementation (…) *)
  | LPAREN INTERFACE iface = scope_body IMPLEMENTATION impl = scope_body RPAREN
    { (Some iface, Some impl) }
  (* Interface only *)
  | LPAREN INTERFACE iface = scope_body RPAREN
    { (Some iface, None) }
  (* Implementation only *)
  | LPAREN IMPLEMENTATION impl = scope_body RPAREN
    { (None, Some impl) }
  (* Anonymous implementation block *)
  | LPAREN ds = decl_list RPAREN
    { (None, Some ds) }
  (* Layout form: handled by the layout preprocessor; the parser only
     sees the paren form after VSEMI insertion. *)

scope_body :
  LPAREN ds = decl_list RPAREN   { ds }

impl_body :
  LPAREN ds = decl_list RPAREN   { ds }

trait_body :
  LPAREN sigs = trait_sig_list RPAREN   { sigs }

trait_sig_list :
  | (* empty *)                              { [] }
  | ts = trait_sig_list sep s = trait_sig    { ts @ [s] }
  | s = trait_sig                            { [s] }

(** Symbolic hook keywords valid in trait sig: all except 'pat' and 'from',
    which have their own dedicated sig forms below. *)
sym_hook_kw :
  | BOP     { HBop }   | UOP    { HUop }   | APP    { HApp }
  | PROJECT { HProject } | ASSIGN { HAssign } | VIEW  { HView }
  | ITER    { HIter }  | GEN    { HGen }

trait_sig :
  (* Symbolic/tick sig: bop + [e]? ':' patterns → result? ('←' default)? *)
  | kw = sym_hook_kw sym = hook_sym ep = effect_param_opt
    COLON ps = pat_csv rt = result_ty_opt def = default_opt
    { { ts_attrs = []; ts_kw = kw
      ; ts_head = HHSym (sym, ep, ps, rt); ts_default = def } }
  (* Pat sig: pat Ctor ':' patterns → result? ('←' default)? *)
  | PAT n = UPPER COLON ps = pat_csv rt = result_ty_opt def = default_opt
    { { ts_attrs = []; ts_kw = HPat
      ; ts_head = HHPat (n, ps, rt); ts_default = def } }
  (* From sig: from [e]? ':' pattern → result? ('←' default)? *)
  | FROM ep = effect_param_opt COLON ps = pat_csv rt = result_ty_opt def = default_opt
    { { ts_attrs = []; ts_kw = HFrom
      ; ts_head = HHPos (ep, ps, rt); ts_default = def } }

default_opt :
  | (* empty *)        { None }
  | LARROW e = expr    { Some e }


(* ── Iso definition ─────────────────────────────────────────────────── *)

from_impl :
  | FROM p = pat LARROW e = expr   { (p, e) }

(** Restricted pattern for iso heads.  Only allows [LOWER] (type variable)
    or [UPPER LOWER*] (applied type constructor with variable arguments).
    This prevents the iso body [(…)] from being greedily consumed as
    constructor arguments when the lookahead after UPPER is LPAREN.
    The S/R conflict on [LOWER] after [UPPER] resolves by default shift,
    so [UPPER LOWER…] correctly gathers all trailing lower-case names. *)
iso_pat :
  | n = LOWER   { PVar (loc $startpos, n) }
  | c = UPPER ns = iso_lower_star
    { PCtor (loc $startpos, c, ns) }

iso_lower_star :
  | (* empty *)                          { [] }
  | ns = iso_lower_star n = LOWER        { ns @ [PVar (loc $startpos, n)] }


(* ── Open filter ────────────────────────────────────────────────────── *)

open_filter_opt :
  | (* empty *)                              { None }
  | LPAREN items = import_item_list RPAREN   { Some (OFOnly items) }
  | kw = LOWER LPAREN items = import_item_list RPAREN
    { if kw = "hiding" then Some (OFHiding items)
      else failwith (Printf.sprintf "open: expected 'hiding', got '%s'" kw) }

import_item_list :
  | i = import_item                                   { [i] }
  | is_ = import_item_list COMMA i = import_item      { is_ @ [i] }

import_item :
  | n = LOWER    { IIName n }
  | t = TICK     { IITick t }
  | n = UPPER    { IIType n }
  | LPAREN o = OP RPAREN   { IIOp o }


(* ── Module path ─────────────────────────────────────────────────────── *)

module_path :
  | n = UPPER                           { [n] }
  | path = module_path DOT n = UPPER    { path @ [n] }


(* ── Extern body ─────────────────────────────────────────────────────── *)

extern_body_opt :
  | (* empty *)                  { None }
  | LARROW s = STRING            { Some (EBSymbol s) }
  | LARROW LOWER s = STRING      { Some (EBExpr ([], s)) }

(* Parameter list for expression-body extern *)
extern_params :
  | (* empty *)                                              { [] }
  | ps = extern_param_list                                   { ps }

extern_param_list :
  | n = LOWER COLON t = fun_ty                               { [(n, t)] }
  | ps = extern_param_list COMMA n = LOWER COLON t = fun_ty { ps @ [(n, t)] }


(* ── Ann (optional type annotation after a name) ─────────────────────── *)

ann_opt :
  | (* empty *)       { None }
  | COLON t = ty      { Some t }


(* ── Misc helpers ───────────────────────────────────────────────────── *)

lower_list :
  | (* empty *)                  { [] }
  | ns = lower_list n = LOWER    { ns @ [n] }

nonempty_ty_list :
  | t = ty_atom                         { [t] }
  | ts = nonempty_ty_list t = ty_atom   { ts @ [t] }

val_lhs :
  | e = expr   { e }
  (* A typed val_lhs (x : T) is handled by a separate decl rule *)


(* ── Handler clauses ─────────────────────────────────────────────────── *)

handler_clause :
  | CTL  n = LOWER ps = pat_list ARROW e = expr
    { HCtl (n, ps, e) }
  | OP_KW n = LOWER ps = pat_list k = LOWER ARROW e = expr
    { HOp (n, ps, k, e) }
  | RETURN p = pat ARROW e = expr
    { HReturn (p, e) }

handler_clauses :
  | c = handler_clause                              { [c] }
  | cs = handler_clauses sep c = handler_clause     { cs @ [c] }


(* ── Expressions ─────────────────────────────────────────────────────── *)

(** [atom] — a self-contained, non-composite value. *)
atom :
  | i = INT      { Lit (loc $startpos, classify_lit i) }
  | f = FLOAT    { Lit (loc $startpos, LFloat f) }
  | s = STRING   { Lit (loc $startpos, LStr s) }
  | s = SYMBOL   { Lit (loc $startpos, LSymbol s) }
  | n = LOWER    { match n with
                   | "true"  -> Lit (loc $startpos, LBool true)
                   | "false" -> Lit (loc $startpos, LBool false)
                   | _       -> Var (loc $startpos, n) }
  | n = UPPER    { if n = "Never" then Lit (loc $startpos, LUnit)   (* placeholder; ⊥ is a type *)
                   else Ctor (loc $startpos, n) }
  | NEVER        { Ctor (loc $startpos, "Never") }
  | t = TICK     { TickVal (loc $startpos, t) }
  (* First-class field name: .x *)
  | DOT n = LOWER   { FieldName (loc $startpos, n) }
  (* Parenthesised / tuple / branch / handler block *)
  | e = paren_expr  { e }
  (* Array literal *)
  | LBRACKET es = array_elems RBRACKET  { Array (loc $startpos, es) }
  (* Record literal *)
  | LBRACE fs = record_field_list RBRACE  { Record (loc $startpos, fs) }
  (* Quoted program `[expr] *)
  | BACKTICK_LBRACKET e = expr RBRACKET  { Quoted (loc $startpos, e) }
  (* Unquoted splice .(expr) *)
  | DOT LPAREN e = expr RPAREN           { Splice (loc $startpos, e) }


(** [expr] — left-recursive chain of atoms and projections.

    Equal-precedence, left-to-right per the Arra spec.
    [ARROW] and [LARROW] do not appear inside [expr] — only in stmt
    position.  This keeps the expression grammar clean. *)
expr :
  | a = atom                                        { a }

  (* Tuple-index access: t.1 *)
  | e = expr DOT i = INT
    { TupleIdx (loc $startpos, e, int_of_string i) }

  (* Field update: r.(x ← v ; y ← w) *)
  | e = expr DOT LPAREN us = update_list RPAREN
    { FieldUpdate (loc $startpos, e, us) }

  (* Postfix unary: x!   x-   (op with no rhs; shifts when next token
     can start an atom, reduces otherwise) *)
  | e = expr o = OP            %prec BELOW_OP
    { Uop (loc $startpos, o, e) }

  (* Binary operator *)
  | l = expr o = OP r = expr   %prec OP
    { Bop (loc $startpos, o, l, r) }

  (* Range: lo .. hi  or  lo .. step .. hi *)
  | lo = expr DOTDOT hi = expr  %prec OP
    { Range (loc $startpos, lo, hi, None) }

  (* As-pattern in expression position: e @ name *)
  | e = expr AT n = LOWER
    { App (loc $startpos, e, Var (loc $startpos, n)) }

  (* Tick application: arr 'map f   or   arr 'rank 1 f *)
  | base = expr t = TICK rhs = tick_rhs
    { let (opt_v, opt_a) = rhs in make_tick_app (loc $startpos) base t opt_v opt_a }

  (* Juxtaposition application (Forth-style: right-hand is the function) *)
  | e = expr a = atom
    { App (loc $startpos, e, a) }


(** [tick_val] — like [atom] but without [LBRACKET … RBRACKET].
    Used in the two-arg tick form so that [LBRACKET] is unambiguously
    treated as the start of a [proj_arg] array-pattern lambda rather
    than an array-literal value argument.  If an array literal is needed
    as a value arg, parenthesise it: [arr 'map ([1;2;3]) f]. *)
tick_val :
  | i = INT      { Lit (loc $startpos, classify_lit i) }
  | f = FLOAT    { Lit (loc $startpos, LFloat f) }
  | s = STRING   { Lit (loc $startpos, LStr s) }
  | s = SYMBOL   { Lit (loc $startpos, LSymbol s) }
  | n = LOWER    { match n with
                   | "true"  -> Lit (loc $startpos, LBool true)
                   | "false" -> Lit (loc $startpos, LBool false)
                   | _       -> Var (loc $startpos, n) }
  | n = UPPER    { if n = "Never" then Lit (loc $startpos, LUnit)
                   else Ctor (loc $startpos, n) }
  | NEVER        { Ctor (loc $startpos, "Never") }
  | t = TICK     { TickVal (loc $startpos, t) }
  | DOT n = LOWER              { FieldName (loc $startpos, n) }
  | e = paren_expr             { e }
  | LBRACE fs = record_field_list RBRACE  { Record (loc $startpos, fs) }
  | BACKTICK_LBRACKET e = expr RBRACKET   { Quoted (loc $startpos, e) }
  | DOT LPAREN e = expr RPAREN            { Splice (loc $startpos, e) }


(** [tick_rhs] — what follows a TICK in a tick-application projection.

    Two-consecutive-lower rule: if the token after the tick is a LOWER
    and the token after *that* is also a LOWER, parse the first as the
    optional value argument and the second as the proj_arg.  Menhir's
    default shift preference implements this correctly. *)
tick_rhs :
  (* Standard one-arg form (or two-arg if a second LOWER follows — shift wins) *)
  | a = proj_arg                  { (None, Some a) }
  (* Two-arg form: value? = first tick_val, proj_arg = second.
     tick_val excludes LBRACKET so that [pat] → proj_arg array-pattern
     lambdas are unambiguous. *)
  | v = tick_val a = proj_arg     { (Some v, Some a) }


(** [proj_arg] — the argument slot in a tick or operator application.
    Includes unparenthesised lambdas for [pattern_atom → program].
    Lambda rules MUST come before their plain-atom counterparts so that
    Menhir's default shift preference chooses the lambda path on ARROW. *)
proj_arg :
  (* Unparenthesised lambda: lower/_ → body  (shift on ARROW beats reduce to Var) *)
  | n = LOWER ARROW e = expr
    { let p = match n with
              | "_"     -> PWild
              | "true"  -> PLit (loc $startpos, LBool true)
              | "false" -> PLit (loc $startpos, LBool false)
              | _       -> PVar (loc $startpos, n)
      in Lam (loc $startpos, { b_pat = p; b_guard = None; b_body = e }) }
  (* Unparenthesised lambda: lower/_ when guard → body *)
  | n = LOWER WHEN g = expr ARROW e = expr
    { let p = match n with
              | "_"     -> PWild
              | "true"  -> PLit (loc $startpos, LBool true)
              | "false" -> PLit (loc $startpos, LBool false)
              | _       -> PVar (loc $startpos, n)
      in Lam (loc $startpos, { b_pat = p; b_guard = Some g; b_body = e }) }
  (* Unparenthesised lambda: Upper → body  (nullary constructor pattern) *)
  | n = UPPER ARROW e = expr
    { Lam (loc $startpos, { b_pat = PCtor (loc $startpos, n, []); b_guard = None; b_body = e }) }
  (* Unparenthesised lambda: Upper when guard → body *)
  | n = UPPER WHEN g = expr ARROW e = expr
    { Lam (loc $startpos, { b_pat = PCtor (loc $startpos, n, []); b_guard = Some g; b_body = e }) }
  (* Unparenthesised lambda: lit → body  (and with guard) *)
  | i = INT ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, classify_lit i); b_guard = None; b_body = e }) }
  | f = FLOAT ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, LFloat f); b_guard = None; b_body = e }) }
  | s = STRING ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, LStr s); b_guard = None; b_body = e }) }
  | s = SYMBOL ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, LSymbol s); b_guard = None; b_body = e }) }
  | i = INT WHEN g = expr ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, classify_lit i); b_guard = Some g; b_body = e }) }
  | f = FLOAT WHEN g = expr ARROW e = expr
    { Lam (loc $startpos, { b_pat = PLit (loc $startpos, LFloat f); b_guard = Some g; b_body = e }) }
  (* Paren-wrapped pattern: (x), (x, y), (Ctor arg), (typed), etc. → body.
     paren_blocks handles comma-separated items so (x, y) → PTuple.
     Shift on ARROW beats the plain grouping reduction. *)
  | LPAREN blocks = paren_blocks RPAREN ARROW e = expr
    { Lam (loc $startpos,
        { b_pat = to_pat "lambda" (make_tuple (loc $startpos) blocks);
          b_guard = None; b_body = e }) }
  | LPAREN blocks = paren_blocks RPAREN WHEN g = expr ARROW e = expr
    { Lam (loc $startpos,
        { b_pat = to_pat "lambda" (make_tuple (loc $startpos) blocks);
          b_guard = Some g; b_body = e }) }
  (* Array pattern: [head; ..rest] → body *)
  | LBRACKET lp = list_pat_body RBRACKET ARROW e = expr
    { Lam (loc $startpos, { b_pat = PArray (loc $startpos, lp); b_guard = None; b_body = e }) }
  | LBRACKET lp = list_pat_body RBRACKET WHEN g = expr ARROW e = expr
    { Lam (loc $startpos, { b_pat = PArray (loc $startpos, lp); b_guard = Some g; b_body = e }) }
  (* Record pattern: {x, y: p, ..} → body *)
  | LBRACE rp = record_pat_body RBRACE ARROW e = expr
    { Lam (loc $startpos, { b_pat = PRecord (loc $startpos, rp); b_guard = None; b_body = e }) }
  | LBRACE rp = record_pat_body RBRACE WHEN g = expr ARROW e = expr
    { Lam (loc $startpos, { b_pat = PRecord (loc $startpos, rp); b_guard = Some g; b_body = e }) }
  | n = LOWER                              { Var (loc $startpos, n) }
  | n = UPPER                              { Ctor (loc $startpos, n) }
  | o = OP                                 { Section (loc $startpos, SBareOp o) }
  | LPAREN e = expr RPAREN                 { e }
  (* Handler block as proj_arg for 'handle: arr 'handle (ctl …) *)
  | LPAREN cs = handler_clauses RPAREN     { Section (loc $startpos, SHandle cs) }


(* ── Tuple expr (used as branch body, allowing bare comma-tuples) ───── *)

tuple_expr :
  | e = expr                              { e }
  | e = expr COMMA rest = tuple_rest      { Tuple (loc $startpos, e :: rest) }

tuple_rest :
  | e = expr                              { [e] }
  | e = expr COMMA rest = tuple_rest      { e :: rest }


(* ── Field update list ───────────────────────────────────────────────── *)

update_list :
  | n = LOWER LARROW e = expr                          { [(n, e)] }
  | us = update_list SEMI n = LOWER LARROW e = expr    { us @ [(n, e)] }


(* ── Array elements ──────────────────────────────────────────────────── *)

array_elems :
  | (* empty *)                             { [] }
  | e = tuple_expr                          { [e] }
  | es = array_elems SEMI e = tuple_expr    { es @ [e] }


(* ── Record fields ───────────────────────────────────────────────────── *)

record_field_list :
  | (* empty *)                              { [] }
  | f = record_field                         { [f] }
  | fs = record_field_list COMMA f = record_field   { fs @ [f] }

record_field :
  | n = LOWER LARROW e = expr   { (n, e) }


(* ── Parenthesised expressions ───────────────────────────────────────── *)

paren_expr :
  (* Unit *)
  | LPAREN RPAREN                          { Lit (loc $startpos, LUnit) }

  (* Handler block: first token is CTL / OP_KW / RETURN *)
  | LPAREN cs = handler_clauses RPAREN     { Section (loc $startpos, SHandle cs) }

  (* Right section: (+1) — OP cannot start atom/paren_item, so unambiguous *)
  | LPAREN o = OP e = expr RPAREN
    { Section (loc $startpos, SRightOp (o, e)) }

  (* Left section: (1+) — shift/reduce with Uop; Menhir default (shift RPAREN)
     means RPAREN after OP is always consumed here, not reduced to Uop. *)
  | LPAREN e = expr o = OP RPAREN
    { Section (loc $startpos, SLeftOp (e, o)) }

  (* Handle section: ('handle (clauses…)) — any tick followed by a handler block.
     Must come before SRightTick/STickVal so it wins the shift/reduce conflict
     when the inner atom is a paren-expr containing handler clauses. *)
  | LPAREN t = TICK LPAREN cs = handler_clauses RPAREN RPAREN
    { let _ = t in Section (loc $startpos, SHandle cs) }

  (* Tick with extra value arg: ('rank 1 f) — two atoms after the tick *)
  | LPAREN t = TICK v = atom a = atom RPAREN
    { Section (loc $startpos, STickVal (t, v, a)) }

  (* Tick right section: ('map f) — one atom after the tick *)
  | LPAREN t = TICK a = atom RPAREN
    { Section (loc $startpos, SRightTick (t, a)) }

  (* Bare tick section: ('map) *)
  | LPAREN t = TICK RPAREN
    { Section (loc $startpos, SBareTick t) }

  (* Tick left section: (arr 'map) — expr followed by tick *)
  | LPAREN e = expr t = TICK RPAREN
    { Section (loc $startpos, SLeftTick (e, t)) }

  (* General form: list of items, possibly comma-separated into a tuple *)
  | LPAREN blocks = paren_blocks RPAREN
    { make_tuple (loc $startpos) blocks }


paren_blocks :
  | b = paren_block                               { [b] }
  | bs = paren_blocks COMMA b = paren_block       { bs @ [b] }


paren_block :
  | items = paren_item_list                       { items }


paren_item_list :
  | i = paren_item                                { [i] }
  | is_ = paren_item_list sep i = paren_item      { is_ @ [i] }


paren_item :
  (* Type signature: name : type *)
  | n = LOWER COLON t = ty
    { PITypeSig (n, t) }

  (* Open: open M  or  open M (f, g)  or  open M hiding (h) — scopes to rest of block *)
  | OPEN path = module_path flt = open_filter_opt
    { PIOpen (path, flt) }

  (* Recursive binding: rec name ← expr  or  rec (pat ← expr; …)
     The single form is restricted to a plain name LHS so that rec LPAREN
     unambiguously signals the block form. *)
  | REC n = LOWER ann = ann_opt LARROW e = expr
    { PIRec [{ rb_pat = PVar (loc $startpos, n); rb_ty = ann; rb_expr = e }] }
  | REC LPAREN bs = rec_binding_list RPAREN
    { PIRec bs }

  (* Branch: expr (when guard)? → body *)
  | lhs = expr WHEN g = expr ARROW body = tuple_expr
    { PIBranch (lhs, Some g, body) }
  | lhs = expr ARROW body = tuple_expr
    { PIBranch (lhs, None, body) }

  (* Bind: expr ← expr *)
  | lhs = expr LARROW rhs = expr
    { PIBind (lhs, rhs) }

  (* Bare expression (including let-like uses of ← via layout) *)
  | e = expr
    { PIExpr e }


(* ── Patterns ────────────────────────────────────────────────────────── *)

(** Full pattern — allows or-patterns, typed patterns, as-patterns. *)
pat :
  | p = pat_or                              { p }
  | p = pat_or AT n = LOWER                { PAs (loc $startpos, p, n) }
  (* Shape annotation suffix: x[n], x[n;m], x[_] — binds tighter than @ *)
  | p = pat_or LBRACKET ds = dim_list RBRACKET  { PShape (loc $startpos, p, ds) }
  (* Typed pattern *)
  | LPAREN p = pat COLON t = ty RPAREN     { PTyped (loc $startpos, p, t) }

(** Or-pattern base *)
pat_or :
  | p = pat_and                             { p }
  | ps = pat_or OP(*|*) p2 = pat_and
    { (* The OP should be "|"; we check it at runtime for cleanliness *)
      match p2 with
      | POr (l, more) -> POr (l, match ps with
          | POr (_, ps') -> ps' @ more | p -> p :: more)
      | _ -> (match ps with
          | POr (l, ps') -> POr (l, ps' @ [p2])
          | _ -> POr (loc $startpos, [ps; p2])) }

pat_and :
  | p = pat_app                             { p }

(** Constructor application pattern *)
pat_app :
  | p = pat_atom                            { p }
  (* Constructor with one arg — spec: upper pattern?
     Nullary case is handled by pat_atom → upper. *)
  | c = UPPER p = pat_atom                  { PCtor (loc $startpos, c, [p]) }

(** Atomic pattern — no further application *)
pat_atom :
  | n = LOWER   { match n with
                  | "_"     -> PWild
                  | "true"  -> PLit (loc $startpos, LBool true)
                  | "false" -> PLit (loc $startpos, LBool false)
                  | _       -> PVar (loc $startpos, n) }
  | n = UPPER   { PCtor (loc $startpos, n, []) }
  | NEVER   { PLit (loc $startpos, LUnit) }   (* ⊥ as literal placeholder *)
  | i = INT     { PLit (loc $startpos, classify_lit i) }
  | f = FLOAT   { PLit (loc $startpos, LFloat f) }
  | s = STRING  { PLit (loc $startpos, LStr s) }
  | s = SYMBOL  { PLit (loc $startpos, LSymbol s) }
  (* First-class field name literal: .x — matches the field-name value itself *)
  | DOT n = LOWER  { PLit (loc $startpos, LField n) }
  (* Tuple / or-pattern / typed pattern in parens *)
  | LPAREN RPAREN           { PLit (loc $startpos, LUnit) }
  | LPAREN p = pat RPAREN   { p }
  (* Tuple pattern *)
  | LPAREN p1 = pat COMMA ps = pat_tuple_rest RPAREN
    { PTuple (loc $startpos, p1 :: ps) }
  (* Array pattern *)
  | LBRACKET lp = list_pat_body RBRACKET
    { PArray (loc $startpos, lp) }
  (* Record pattern *)
  | LBRACE rp = record_pat_body RBRACE
    { PRecord (loc $startpos, rp) }
  (* View pattern *)
  | LPAREN e = expr ARROW p = pat RPAREN
    { PView (loc $startpos, e, p) }

pat_tuple_rest :
  | p = pat                                { [p] }
  | p = pat COMMA ps = pat_tuple_rest      { p :: ps }

list_pat_body :
  | (* empty — [  ] *)
    { { lp_prefix = []; lp_rest = None; lp_suffix = [] } }
  | ps = list_pat_prefix
    { { lp_prefix = ps; lp_rest = None; lp_suffix = [] } }
  | ps = list_pat_prefix SEMI DOTDOT rest = rest_name
    { { lp_prefix = ps; lp_rest = rest; lp_suffix = [] } }
  | ps = list_pat_prefix SEMI DOTDOT rest = rest_name SEMI ss = list_pat_prefix
    { { lp_prefix = ps; lp_rest = rest; lp_suffix = ss } }
  | DOTDOT rest = rest_name
    { { lp_prefix = []; lp_rest = rest; lp_suffix = [] } }
  | DOTDOT rest = rest_name SEMI ss = list_pat_prefix
    { { lp_prefix = []; lp_rest = rest; lp_suffix = ss } }

list_pat_prefix :
  | p = pat                                    { [p] }
  | ps = list_pat_prefix SEMI p = pat          { ps @ [p] }

rest_name :
  | (* _ *) { None }
  | n = LOWER   { Some n }

record_pat_body :
  | (* empty *)
    { { rp_fields = []; rp_open = None } }
  | fs = record_pat_fields
    { { rp_fields = fs; rp_open = None } }
  | fs = record_pat_fields COMMA DOTDOT
    { { rp_fields = fs; rp_open = Some "" } }
  | fs = record_pat_fields COMMA DOTDOT n = LOWER
    { { rp_fields = fs; rp_open = Some n } }
  | DOTDOT
    { { rp_fields = []; rp_open = Some "" } }

record_pat_fields :
  | f = record_pat_field                                { [f] }
  | fs = record_pat_fields COMMA f = record_pat_field  { fs @ [f] }

record_pat_field :
  | n = LOWER                        { (n, None) }
  | n = LOWER COLON p = pat          { (n, Some p) }


(* ── Types ───────────────────────────────────────────────────────────── *)

(** Full type with optional universal quantification. *)
ty :
  | FORALL tvs = tvar_list1 DARROW t = row_ty   { TConstrained (tvs, t) }
  | EXISTS LPAREN n = LOWER COLON k_ty = ty COMMA e = expr RPAREN t = ty_atom
    { TSigma (n, k_ty, e, t) }
  | t = row_ty                                  { t }

(** Row type operations: A ++ B (concat) and A - .field (field removal).
    Placed above fun_ty so row ops bind tighter than ARROW.
    field_ty uses fun_ty directly to keep {… | rest} unambiguous. *)
row_ty :
  | t = fun_ty                        { t }
  | t1 = row_ty o = OP t2 = fun_ty   { match o with
      | "++" -> TRowConcat (t1, t2)
      | _    -> failwith ("unknown row operator: " ^ o) }
  | t1 = row_ty o = OP DOT n = LOWER { match o with
      | "-" -> TRowSub (t1, n)
      | _   -> failwith ("unknown row operator: " ^ o) }

(** Function type: chains via right-recursion. *)
fun_ty :
  | t = app_ty ARROW eff = eff_opt ft = fun_ty
    { TFun (t, eff, ft) }
  | t = app_ty                        { t }

eff_opt :
  | (* empty *)               { None }
  | LEFF es = eff_entries REFF   { Some (ERow es) }
  | LEFF REFF                    { Some EPure }

eff_entries :
  | e = eff_entry                           { [e] }
  | es = eff_entries OP(*|*) e = eff_entry  { es @ [e] }

eff_entry :
  | n = UPPER ts = ty_atom_list   { EENamed (n, ts) }
  | n = UPPER                     { EENamed (n, []) }
  | n = LOWER                     { EEVar n }

(** Type application by juxtaposition (left-associative). *)
app_ty :
  | t = ty_atom                       { t }
  | f = app_ty t = ty_atom            { TApp (f, t) }

(** Atomic type. *)
ty_atom :
  | n = UPPER   { TName n }
  | n = LOWER   { TVar n }
  | NEVER       { TNever }
  (* Array type: T[n; m; …] *)
  | t = ty_atom LBRACKET ds = dim_list RBRACKET  { TArr (t, ds) }
  (* Dynamic array: T[] *)
  | t = ty_atom LBRACKET RBRACKET   { TDyn t }
  (* Unit type: () *)
  | LPAREN RPAREN                   { TUnit }
  (* Grouped / product type *)
  | LPAREN t = fun_ty RPAREN        { t }
  | LPAREN t1 = fun_ty COMMA ts = fun_ty_list RPAREN
    { TProduct (t1 :: ts) }
  (* Record type *)
  | LBRACE fs = field_ty_list RBRACE   { TRecord (RClosed fs) }
  | LBRACE fs = field_ty_list OP n = LOWER RBRACE
    (* The OP should be "|" for open record row tail *)
    { TRecord (ROpen (fs, n)) }

fun_ty_list :
  | t = fun_ty                            { [t] }
  | ts = fun_ty_list COMMA t = fun_ty     { ts @ [t] }

field_ty_list :
  | f = field_ty                              { [f] }
  | fs = field_ty_list COMMA f = field_ty    { fs @ [f] }

field_ty :
  (* Uses fun_ty (not ty/row_ty) so that OP "|" after a field list is
     still available for the open-record tail {… | rest} syntax. *)
  | n = LOWER COLON t = fun_ty   { { fname = n; fty = t } }


(* ── Dimension expressions ───────────────────────────────────────────── *)

dim_list :
  | d = dim                          { [d] }
  | ds = dim_list SEMI d = dim       { ds @ [d] }

dim :
  | n = LOWER    { DVar n }
  | i = INT      { DLit (int_of_string i) }
  | d1 = dim OP d2 = dim
    { match $2 with
      | "+" -> DAdd (d1, d2)
      | "-" -> DSub (d1, d2)
      | "*" -> (match d1 with
          | DLit k -> DMul (k, d2)
          | _      -> DMulD (d1, d2))
      | _   -> failwith ("unknown dim operator: " ^ $2) }
  | LPAREN d = dim RPAREN   { d }
