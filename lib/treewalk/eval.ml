(** Treewalk interpreter — the definition of correct.
    Interprets Core IR directly using OCaml 5's native effect system.
    No optimization, no fusion. Oracle for all compiler passes.
    See doc/BACKEND.md §1. *)

open Core_ir.Ir

(** Runtime values *)
type value =
  | VInt    of int
  | VFloat  of float
  | VBool   of bool
  | VByte   of int
  | VNat    of int
  | VUnit
  | VArr    of value array
  | VPtr    of value ref
  | VTuple  of value list
  | VRecord of (string * value) list
  | VClos   of closure
  | VCtor   of string * value list
  (** Wraps an OCaml 5 continuation so it is callable as a first-class value.
      Created only by HOp handlers; cannot appear in surface programs. *)
  | VCont   of (value -> value)
  (** Mutable cell used internally by LetRec backpatching.
      lookup always dereferences these; they never escape into results. *)
  | VRef    of value ref

and closure =
  { params : id list        (** types erased — runtime doesn't need them *)
  ; body   : expr
  ; env    : env
  }

and env = (id * value) list

exception EvalError of string

let lookup env id =
  match List.assoc_opt id env with
  | Some (VRef r) -> !r   (* dereference letrec backpatch cell *)
  | Some v -> v
  | None   -> raise (EvalError ("unbound: " ^ id.name))

(** OCaml 5 effects for Arra's effect system.
    Each named Arra effect maps to an OCaml effect.
    See doc/BACKEND.md §1 and doc/semantics/05_EFFECTS.md. *)
type _ Effect.t +=
  | Perform : string * value list -> value Effect.t
  | Fail    : string -> value Effect.t

(** Evaluate a Core expression in an environment *)
let rec eval (env : env) (e : expr) : value =
  match e with
  | Lit l          -> eval_lit l
  | Var id         -> lookup env id

  (** Lam: params carry types in Core; strip to ids for closure *)
  | Lam (ps, body) ->
    let params = List.map fst ps in
    VClos { params; body; env }

  (** TyLam/TyApp: eliminated by specialization pass; should not appear at runtime *)
  | TyLam (_, _, e)  -> eval env e
  | TyApp (e, _)     -> eval env e

  (** Let: type annotation present in Core; ignore at runtime *)
  | Let (id, _ty, e1, e2) ->
    let v = eval env e1 in
    eval ((id, v) :: env) e2

  | App (f, args) ->
    let fv  = eval env f in
    let avs = List.map (eval env) args in
    apply fv avs

  (** LetRec: backpatching letrec.
      1. Allocate a VRef cell for each binding.
      2. Extend env with the cells so RHSs can refer to each other.
      3. Evaluate each RHS (expected to be a Lam) in the extended env
         and write the result into its cell.
      All VRef lookups auto-deref, so cells are invisible to the program. *)
  | LetRec (bindings, body) ->
    let cells    = List.map (fun (id, _, _) -> (id, ref VUnit)) bindings in
    let ext_env  = List.map (fun (id, cell) -> (id, VRef cell)) cells @ env in
    List.iter2 (fun (_, cell) (_, _, e) ->
      cell := eval ext_env e
    ) cells bindings;
    eval ext_env body

  (** Case: carries explicit return type; ignore at runtime *)
  | Case (scrut, _ty, branches) ->
    let v = eval env scrut in
    eval_case env v branches

  (** Constructor application *)
  | Con (name, args) ->
    VCtor (name, List.map (eval env) args)

  (** Tuple construction *)
  | Tuple exprs ->
    VTuple (List.map (eval env) exprs)

  (** Named record construction *)
  | Record fields ->
    VRecord (List.map (fun (n, e) -> (n, eval env e)) fields)

  (** Array literal — dim annotation ignored at runtime *)
  | Array (exprs, _dim) ->
    VArr (Array.of_list (List.map (eval env) exprs))

  (** Tuple field access, 1-indexed *)
  | ProjI (e, n) ->
    (match eval env e with
     | VTuple vs ->
       (match List.nth_opt vs (n - 1) with
        | Some v -> v
        | None   -> raise (EvalError (Printf.sprintf "tuple index %d out of range" n)))
     | _ -> raise (EvalError "ProjI: not a tuple"))

  (** Record field access *)
  | ProjF (e, name) ->
    (match eval env e with
     | VRecord fields ->
       (match List.assoc_opt name fields with
        | Some v -> v
        | None   -> raise (EvalError ("ProjF: missing field " ^ name)))
     | _ -> raise (EvalError ("ProjF: not a record")))

  | Prim (p, args) ->
    let vs = List.map (eval env) args in
    eval_prim p vs

  | Perform (op, args) ->
    let vs = List.map (eval env) args in
    Effect.perform (Perform (op, vs))

  | Handle (body, clauses) ->
    eval_handle env body clauses

  (** Ann: type annotation survives Core passes; ignore at runtime *)
  | Ann (e, _ty) -> eval env e

  | Extern ({ er_body = Some _; _ }, _) ->
    raise (EvalError
      "expression-body extern: compile-only, cannot be interpreted.")
  | Extern ({ er_symbol; er_body = None; _ }, _) ->
    raise (EvalError
      ("call-form extern '" ^ er_symbol ^ "': \
        FFI is compile-only in the initial implementation."))

and eval_lit = function
  | LInt n   -> VInt n
  | LFloat f -> VFloat f
  | LBool b  -> VBool b
  | LByte b  -> VByte b
  | LUnit    -> VUnit

and apply fv avs =
  match fv with
  | VClos { params; body; env } ->
    let env' = List.combine params avs @ env in
    eval env' body
  | VCont f ->
    (match avs with
     | [arg] -> f arg
     | _ -> raise (EvalError "VCont: continuation takes exactly one argument"))
  | _ -> raise (EvalError "apply: not a function")

and eval_prim p vs =
  match p, vs with
  (** Int arithmetic *)
  | AddInt, [VInt a; VInt b] -> VInt (a + b)
  | SubInt, [VInt a; VInt b] -> VInt (a - b)
  | MulInt, [VInt a; VInt b] -> VInt (a * b)
  | DivInt, [VInt a; VInt b] -> VInt (a / b)
  | ModInt, [VInt a; VInt b] -> VInt (a mod b)
  | NegInt, [VInt a]         -> VInt (-a)
  (** Float arithmetic *)
  | AddFloat, [VFloat a; VFloat b] -> VFloat (a +. b)
  | SubFloat, [VFloat a; VFloat b] -> VFloat (a -. b)
  | MulFloat, [VFloat a; VFloat b] -> VFloat (a *. b)
  | DivFloat, [VFloat a; VFloat b] -> VFloat (a /. b)
  | NegFloat, [VFloat a]           -> VFloat (-.a)
  (** Bitwise Int *)
  | AndInt, [VInt a; VInt b] -> VInt (a land b)
  | OrInt,  [VInt a; VInt b] -> VInt (a lor b)
  | XorInt, [VInt a; VInt b] -> VInt (a lxor b)
  | NotInt, [VInt a]         -> VInt (lnot a)
  | ShlInt, [VInt a; VInt b] -> VInt (a lsl b)
  | ShrInt, [VInt a; VInt b] -> VInt (a asr b)
  (** Int comparison *)
  | EqInt, [VInt a; VInt b] -> VBool (a = b)
  | LtInt, [VInt a; VInt b] -> VBool (a < b)
  | LeInt, [VInt a; VInt b] -> VBool (a <= b)
  | GtInt, [VInt a; VInt b] -> VBool (a > b)
  | GeInt, [VInt a; VInt b] -> VBool (a >= b)
  (** Float comparison *)
  | EqFloat, [VFloat a; VFloat b] -> VBool (a = b)
  | LtFloat, [VFloat a; VFloat b] -> VBool (a < b)
  | LeFloat, [VFloat a; VFloat b] -> VBool (a <= b)
  (** Bool *)
  | BNot, [VBool b]          -> VBool (not b)
  | BAnd, [VBool a; VBool b] -> VBool (a && b)
  | BOr,  [VBool a; VBool b] -> VBool (a || b)
  | BEq,  [VBool a; VBool b] -> VBool (a = b)
  (** Coercions *)
  | IntToFloat,  [VInt n]   -> VFloat (float_of_int n)
  | FloatToInt,  [VFloat f] -> VInt (int_of_float f)
  | UnsafeCoerce,[v]        -> v
  (** Arrays *)
  | VecGet, [VArr arr; VInt i] ->
    if i < 0 || i >= Array.length arr
    then raise (EvalError "VecGet: index out of bounds")
    else arr.(i)
  | VecSet, [VArr arr; VInt i; v] ->
    if i < 0 || i >= Array.length arr
    then raise (EvalError "VecSet: index out of bounds")
    else let arr' = Array.copy arr in arr'.(i) <- v; VArr arr'
  | VecLen, [VArr arr] -> VNat (Array.length arr)
  | VecAlloc, [VNat n] -> VArr (Array.make n VUnit)
  | VecCopy,  [VArr arr] -> VArr (Array.copy arr)
  (** Assertions *)
  | AssertDim, [VBool true]  -> VUnit
  | AssertDim, [VBool false] ->
    raise (EvalError "dimension assertion failed")
  | AssertBounds, [VNat i; VNat n] ->
    if i < n then VUnit
    else raise (EvalError (Printf.sprintf "bounds check failed: %d >= %d" i n))
  | Panic, [VCtor (msg, [])] ->
    raise (EvalError ("panic: " ^ msg))
  (** SOAC fusion markers — treewalk interprets as plain structural recursion *)
  | VecMap, [VArr arr; (VClos _ as f)] ->
    VArr (Array.map (fun x -> apply f [x]) arr)
  | VecFilter, [VArr arr; (VClos _ as f)] ->
    VArr (Array.of_list
      (Array.to_list arr
       |> List.filter (fun x -> match apply f [x] with
           | VBool b -> b
           | _ -> raise (EvalError "VecFilter: predicate must return Bool"))))
  | VecFold, [VArr arr; (VClos _ as f); init] ->
    Array.fold_left (fun acc x -> apply f [acc; x]) init arr
  | VecScan, [VArr arr; (VClos _ as f); init] ->
    let n = Array.length arr in
    let out = Array.make n VUnit in
    let acc = ref init in
    for i = 0 to n - 1 do
      acc := apply f [!acc; arr.(i)];
      out.(i) <- !acc
    done;
    VArr out
  | VecZip, [VArr a; VArr b] ->
    let n = min (Array.length a) (Array.length b) in
    VArr (Array.init n (fun i -> VTuple [a.(i); b.(i)]))
  | VecMapMaybe, [VArr arr; (VClos _ as f)] ->
    let results = Array.to_list arr
      |> List.filter_map (fun x -> match apply f [x] with
          | VCtor ("Some", [v]) -> Some v
          | VCtor ("None", [])  -> None
          | _ -> raise (EvalError "VecMapMaybe: function must return Option")) in
    VArr (Array.of_list results)
  | VecFlatMap, [VArr arr; (VClos _ as f)] ->
    let parts = Array.to_list arr
      |> List.map (fun x -> match apply f [x] with
          | VArr inner -> Array.to_list inner
          | _ -> raise (EvalError "VecFlatMap: function must return array")) in
    VArr (Array.of_list (List.concat parts))
  | VecConcat, [VArr a; VArr b] ->
    VArr (Array.append a b)
  | p, _ ->
    let name = match p with
      | VecMap -> "VecMap" | VecFilter -> "VecFilter"
      | VecFold -> "VecFold" | VecScan -> "VecScan"
      | _ -> "prim"
    in
    raise (EvalError ("prim " ^ name ^ ": wrong arity or types"))

and eval_handle env body clauses =
  Effect.Deep.match_with
    (eval env) body
    { Effect.Deep.retc = (fun v ->
        (* HReturn clause transforms the return value if present *)
        let ret_clause = List.find_opt (function HReturn _ -> true | _ -> false) clauses in
        (match ret_clause with
         | Some (HReturn ((id, _ty), expr)) ->
           eval ((id, v) :: env) expr
         | _ -> v))
    ; exnc = raise
    ; effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Perform (op, args) ->
          (* Look for HCtl or HOp clause matching the operation name *)
          let clause = List.find_opt (function
            | HCtl (name, _, _) -> name = op
            | HOp  (name, _, _, _) -> name = op
            | HReturn _ -> false) clauses in
          (match clause with
           | None -> None
           | Some (HCtl (_, (id, _ty), expr)) ->
             Some (fun (_k : (a, value) Effect.Deep.continuation) ->
               let env' = (id, match args with [v] -> v | vs -> VTuple vs) :: env in
               eval env' expr)
           | Some (HOp (_, (id, _ty), (k_id, _k_ty), expr)) ->
             Some (fun (k : (a, value) Effect.Deep.continuation) ->
               let k_val = VCont (Effect.Deep.continue k) in
               let env' = (id, match args with [v] -> v | vs -> VTuple vs)
                          :: (k_id, k_val)
                          :: env in
               eval env' expr)
           | Some (HReturn _) -> None)
        | _ -> None
    }

and eval_case env v branches =
  match branches with
  | [] -> raise (EvalError "non-exhaustive case")
  | { br_pat; br_body } :: rest ->
    (match match_pat br_pat v with
     | Some bindings -> eval (bindings @ env) br_body
     | None          -> eval_case env v rest)

and match_pat pat v =
  match pat, v with
  | PWild, _  -> Some []
  | PVar (id, _ty), v -> Some [(id, v)]
  | PCon (c, pats), VCtor (c', vs) when c = c' && List.length pats = List.length vs ->
    let sub = List.map2 match_pat pats vs in
    if List.for_all Option.is_some sub
    then Some (List.concat (List.filter_map Fun.id sub))
    else None
  | PTuple pats, VTuple vs when List.length pats = List.length vs ->
    let sub = List.map2 match_pat pats vs in
    if List.for_all Option.is_some sub
    then Some (List.concat (List.filter_map Fun.id sub))
    else None
  | PRec field_pats, VRecord field_vals ->
    let results = List.filter_map (fun (name, pat) ->
      match List.assoc_opt name field_vals with
      | None   -> None
      | Some v -> match_pat pat v) field_pats in
    if List.length results = List.length field_pats
    then Some (List.concat results)
    else None
  | PLit (LInt n),   VInt m   when n = m -> Some []
  | PLit (LFloat f), VFloat g when f = g -> Some []
  | PLit (LBool b),  VBool b' when b = b' -> Some []
  | PLit (LByte b),  VByte b' when b = b' -> Some []
  | PLit LUnit,      VUnit                -> Some []
  | PAnn (pat, _ty), v -> match_pat pat v
  | _ -> None
