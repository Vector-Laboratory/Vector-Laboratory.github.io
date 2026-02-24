(** Core IR QCheck generator.

    Produces well-typed Core IR expressions for fuzz testing the treewalk
    interpreter. All generated programs are expected to evaluate without
    raising EvalError — a test failure on a generated program means either
    a generator bug (not well-typed) or an evaluator bug.

    Type-directed generation over a tractable subset:
      Base      : Int, Bool, Float, Unit
      Compound  : TTuple [base; base], TFun (base, base)

    Excluded intentionally: effects, arrays, records, existentials, type
    variables.  Those are exercised by hand-written cases in test_core.ml. *)

open QCheck2
open Core_ir.Ir

(* ── Types ─────────────────────────────────────────────────────────────── *)

let base_tys : ty list = [TCon "Int"; TCon "Bool"; TCon "Float"; TCon "Unit"]

let gen_base_ty : ty Gen.t = Gen.oneof_list base_tys

let gen_ty : ty Gen.t =
  Gen.oneof_weighted
    [ 4, gen_base_ty
    ; 2, Gen.map2 (fun a b -> TTuple [a; b]) gen_base_ty gen_base_ty
    ; 1, Gen.map2 (fun a b -> TFun (a, b))  gen_base_ty gen_base_ty
    ]

(* ── Literals ───────────────────────────────────────────────────────────── *)

(** Generate a literal of the given type.
    Floats are derived from small ints to avoid NaN/inf in comparisons. *)
let gen_lit (ty : ty) : lit Gen.t =
  match ty with
  | TCon "Int"   -> Gen.map (fun n -> LInt   n)               Gen.int_small
  | TCon "Bool"  -> Gen.map (fun b -> LBool  b)               Gen.bool
  | TCon "Float" -> Gen.map (fun n -> LFloat (float_of_int n)) Gen.int_small
  | _            -> Gen.return LUnit

(* ── Expressions ────────────────────────────────────────────────────────── *)

(** All variables in context whose type equals [ty]. *)
let vars_of (ctx : (id * ty) list) (ty : ty) : expr list =
  List.filter_map
    (fun (id, t) -> if t = ty then Some (Var id) else None) ctx

(** Base (fuel = 0) generator for [ty].
    Always produces a well-typed expression; never recurses into compound
    types deeper than one level. *)
let rec gen_base (ctx : (id * ty) list) (ty : ty) : expr Gen.t =
  let from_vars base_g =
    match vars_of ctx ty with
    | []  -> base_g
    | vs  -> Gen.oneof_weighted [1, base_g; 3, Gen.oneof_list vs]
  in
  match ty with
  | TTuple [t1; t2] ->
    from_vars
      (Gen.map2 (fun e1 e2 -> Tuple [e1; e2])
        (gen_base ctx t1) (gen_base ctx t2))
  | TFun (a, b) ->
    let x = fresh "p" in
    from_vars
      (Gen.map (fun body -> Lam ([(x, a)], body))
        (gen_base ((x, a) :: ctx) b))
  | _ ->
    from_vars (Gen.map (fun l -> Lit l) (gen_lit ty))

(** Main type-directed generator.
    [fuel] bounds expression depth; recursive constructors decrease it. *)
and gen_expr (ctx : (id * ty) list) (ty : ty) (fuel : int) : expr Gen.t =
  if fuel <= 0 then gen_base ctx ty
  else
    let f  = fuel - 1 in
    let base = gen_base ctx ty in

    (* Let: any type *)
    let let_g =
      Gen.bind gen_base_ty (fun t ->
        Gen.bind (gen_expr ctx t f) (fun e1 ->
          let x = fresh "v" in
          Gen.map (fun body -> Let (x, t, e1, body))
            (gen_expr ((x, t) :: ctx) ty f)))
    in

    (* Ann: any type *)
    let ann_g = Gen.map (fun e -> Ann (e, ty)) (gen_expr ctx ty f) in

    (* Type-specific constructors *)
    let specific : expr Gen.t list = match ty with
      | TCon "Int" ->
        let arith op =
          Gen.map2 (fun a b -> Prim (op, [a; b]))
            (gen_expr ctx ty f) (gen_expr ctx ty f)
        in
        let cond_g =
          Gen.bind (gen_expr ctx (TCon "Bool") f) (fun cond ->
            Gen.map2
              (fun e1 e2 ->
                Case (cond, ty,
                  [ { br_pat = PLit (LBool true);  br_body = e1 }
                  ; { br_pat = PLit (LBool false); br_body = e2 } ]))
              (gen_expr ctx ty f) (gen_expr ctx ty f))
        in
        let proj_g =
          Gen.bind gen_base_ty (fun t2 ->
            Gen.map (fun tup -> ProjI (tup, 1))
              (gen_expr ctx (TTuple [ty; t2]) f))
        in
        [ arith AddInt; arith SubInt; arith MulInt; cond_g; proj_g ]

      | TCon "Bool" ->
        let cmp op =
          Gen.map2 (fun a b -> Prim (op, [a; b]))
            (gen_expr ctx (TCon "Int") f) (gen_expr ctx (TCon "Int") f)
        in
        let bool2 op =
          Gen.map2 (fun a b -> Prim (op, [a; b]))
            (gen_expr ctx ty f) (gen_expr ctx ty f)
        in
        [ cmp EqInt; cmp LtInt; cmp LeInt
        ; Gen.map (fun e -> Prim (BNot, [e])) (gen_expr ctx ty f)
        ; bool2 BAnd; bool2 BOr
        ]

      | TCon "Float" ->
        let arith op =
          Gen.map2 (fun a b -> Prim (op, [a; b]))
            (gen_expr ctx ty f) (gen_expr ctx ty f)
        in
        [ arith AddFloat; arith MulFloat
        ; Gen.map (fun e -> Prim (IntToFloat, [e])) (gen_expr ctx (TCon "Int") f)
        ]

      | TCon "Unit" -> []

      | TTuple [t1; t2] ->
        [ Gen.map2 (fun e1 e2 -> Tuple [e1; e2])
            (gen_expr ctx t1 f) (gen_expr ctx t2 f) ]

      | TFun (a, b) ->
        let x = fresh "p" in
        [ Gen.map (fun body -> Lam ([(x, a)], body))
            (gen_expr ((x, a) :: ctx) b f) ]

      | _ -> []
    in
    let weighted = List.map (fun g -> (1, g)) (let_g :: ann_g :: specific) in
    Gen.oneof_weighted ((5, base) :: weighted)

(* ── Top-level generators ───────────────────────────────────────────────── *)

(** Closed expression of [ty] with depth bounded randomly in [0, 4]. *)
let gen_closed (ty : ty) : expr Gen.t =
  Gen.sized_size (Gen.int_range 0 4) (gen_expr [] ty)

let gen_closed_int  : expr Gen.t = gen_closed (TCon "Int")
let gen_closed_bool : expr Gen.t = gen_closed (TCon "Bool")
let gen_closed_float : expr Gen.t = gen_closed (TCon "Float")

(** A random base type paired with a well-typed closed expression of that type. *)
let gen_typed_closed : (ty * expr) Gen.t =
  Gen.bind gen_base_ty (fun ty -> Gen.map (fun e -> (ty, e)) (gen_closed ty))
