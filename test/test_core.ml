(** QCheck test suite — treewalk oracle tests.
    See doc/BACKEND.md §7 for the harness specification.

    Focus: subtle interpreter behaviour — effects, closures, letrec, patterns.
    Prim arithmetic is not tested here (obviously correct by inspection). *)

open QCheck2
open Treewalk.Eval   (* value types, eval, eval_prim, EvalError *)
open Core_ir.Ir      (* expr constructors win over any Eval shadows *)

(* ── helpers ────────────────────────────────────────────────────────────── *)

let empty_env = []
let ty0 = TCon "T"  (* dummy type annotation — runtime ignores it *)

let eval_pure e =
  try Some (eval empty_env e)
  with EvalError _ -> None

(** Deterministic unit test wrapped in QCheck for uniform output. *)
let unit_test ~name f =
  Test.make ~name ~count:1 Gen.unit (fun () -> f ())

(** Build a single-argument lambda *)
let lam1 id body = Lam ([(id, ty0)], body)

(** Build a Handle expression with a single clause list *)
let handle body clauses = Handle (body, clauses)

(* ── existing prim tests (kept for regression) ──────────────────────────── *)

let test_iadd =
  Test.make ~name:"iadd_commutes"
    Gen.(pair int_small int_small)
    (fun (a, b) ->
      let ea = Prim (AddInt, [Lit (LInt a); Lit (LInt b)]) in
      let eb = Prim (AddInt, [Lit (LInt b); Lit (LInt a)]) in
      eval_pure ea = eval_pure eb)

let test_map_length =
  Test.make ~name:"map_identity_preserves_length"
    Gen.(array_size (int_range 0 32) int_small)
    (fun arr ->
      let varr = VArr (Array.map (fun x -> VInt x) arr) in
      let x_id = fresh "x" in
      let id_fn = VClos { params = [x_id]; body = Var x_id; env = [] } in
      match eval_prim VecMap [varr; id_fn] with
      | VArr out -> Array.length out = Array.length arr
      | _        -> false
      | exception EvalError _ -> false)

let test_fold_sum =
  Test.make ~name:"fold_sum_matches_stdlib"
    Gen.(array_size (int_range 0 16) int_small)
    (fun arr ->
      let expected = Array.fold_left (+) 0 arr in
      let varr = VArr (Array.map (fun x -> VInt x) arr) in
      let acc_id = fresh "acc" in
      let x_id   = fresh "x" in
      let add_fn = VClos
        { params = [acc_id; x_id]
        ; body   = Prim (AddInt, [Var acc_id; Var x_id])
        ; env    = []
        } in
      match eval_prim VecFold [varr; add_fn; VInt 0] with
      | VInt n -> n = expected
      | _      -> false
      | exception EvalError _ -> false)

(* ── Ann passthrough ────────────────────────────────────────────────────── *)

(* Ann is a type annotation that all passes preserve; runtime ignores it. *)
let test_ann =
  unit_test ~name:"ann_is_transparent" (fun () ->
    eval empty_env (Ann (Lit (LInt 42), TCon "Int")) = VInt 42)

(* ── Tuple / record projection ──────────────────────────────────────────── *)

(* ProjI is 1-indexed per Core spec *)
let test_proj_tuple =
  unit_test ~name:"ProjI_1indexed" (fun () ->
    let e = ProjI (Tuple [Lit (LInt 10); Lit (LInt 20); Lit (LInt 30)], 2) in
    eval empty_env e = VInt 20)

let test_proj_record =
  unit_test ~name:"ProjF_by_name" (fun () ->
    let e = ProjF (Record [("x", Lit (LInt 5)); ("y", Lit (LInt 6))], "y") in
    eval empty_env e = VInt 6)

(* ── Closure capture ────────────────────────────────────────────────────── *)

(* adder n x = n + x.  Verify the closure over n is correct. *)
let test_closure_capture =
  Test.make ~name:"closure_captures_env"
    Gen.int_small
    (fun n ->
      let n_id   = fresh "n" in
      let x_id   = fresh "x" in
      let adder  = lam1 n_id (lam1 x_id (Prim (AddInt, [Var n_id; Var x_id]))) in
      let e =
        Let (fresh "f", ty0, App (adder, [Lit (LInt n)]),
          App (Var (fresh "f"), [Lit (LInt 1)]))
      in
      (* We build it properly: bind f first, then call it *)
      let f_id = fresh "f" in
      let e2 =
        Let (f_id, ty0, App (adder, [Lit (LInt n)]),
          App (Var f_id, [Lit (LInt 1)]))
      in
      ignore e;
      eval empty_env e2 = VInt (n + 1))

(* ── LetRec ─────────────────────────────────────────────────────────────── *)

(* Factorial — the canonical letrec smoke test.
   fact 0 = 1, fact n = n * fact (n-1) *)
let test_letrec_fact =
  Test.make ~name:"letrec_factorial"
    Gen.(int_range 0 10)
    (fun n ->
      let fact_id = fresh "fact" in
      let m_id    = fresh "m" in
      let fact_body =
        Case (Var m_id, ty0,
          [ { br_pat = PLit (LInt 0); br_body = Lit (LInt 1) }
          ; { br_pat = PVar (m_id, ty0)
            ; br_body =
                Prim (MulInt,
                  [ Var m_id
                  ; App (Var fact_id,
                      [Prim (SubInt, [Var m_id; Lit (LInt 1)])]) ]) }
          ])
      in
      let e = LetRec
        ([ (fact_id, ty0, lam1 m_id fact_body) ],
         App (Var fact_id, [Lit (LInt n)]))
      in
      let expected = ref 1 in
      for i = 1 to n do expected := !expected * i done;
      eval empty_env e = VInt !expected)

(* Mutual recursion: even/odd via two co-recursive functions *)
let test_letrec_mutual =
  Test.make ~name:"letrec_mutual_even_odd"
    Gen.(int_range 0 20)
    (fun n ->
      let even_id = fresh "even" in
      let odd_id  = fresh "odd"  in
      let k_id    = fresh "k"    in
      let is_zero_branch f =
        Case (Var k_id, ty0,
          [ { br_pat  = PLit (LInt 0); br_body = Lit (LBool true) }
          ; { br_pat  = PVar (k_id, ty0)
            ; br_body = App (Var f,
                [Prim (SubInt, [Var k_id; Lit (LInt 1)])]) } ])
      in
      let e = LetRec
        ([ (even_id, ty0, lam1 k_id (is_zero_branch odd_id))
         ; (odd_id,  ty0,
            lam1 k_id
              (Case (Var k_id, ty0,
                [ { br_pat  = PLit (LInt 0); br_body = Lit (LBool false) }
                ; { br_pat  = PVar (k_id, ty0)
                  ; br_body = App (Var even_id,
                      [Prim (SubInt, [Var k_id; Lit (LInt 1)])]) } ])))
         ],
         App (Var even_id, [Lit (LInt n)]))
      in
      eval empty_env e = VBool (n mod 2 = 0))

(* ── Pattern matching ───────────────────────────────────────────────────── *)

(* Nested constructor patterns: head + second element of Cons list *)
let test_nested_con_pat =
  unit_test ~name:"nested_constructor_pattern" (fun () ->
    let h_id = fresh "h" in
    let s_id = fresh "s" in
    let list_val =
      Con ("Cons", [Lit (LInt 1);
        Con ("Cons", [Lit (LInt 2); Con ("Nil", [])])]) in
    let e = Case (list_val, ty0,
      [ { br_pat =
            PCon ("Cons",
              [ PVar (h_id, ty0)
              ; PCon ("Cons", [PVar (s_id, ty0); PWild]) ])
        ; br_body = Prim (AddInt, [Var h_id; Var s_id]) }
      ]) in
    eval empty_env e = VInt 3)

(* Record pattern: destructure two fields and add them *)
let test_record_pat =
  unit_test ~name:"record_pattern_destructure" (fun () ->
    let a_id = fresh "a" in
    let b_id = fresh "b" in
    let e = Case (
      Record [("x", Lit (LInt 3)); ("y", Lit (LInt 4))],
      ty0,
      [ { br_pat  = PRec [("x", PVar (a_id, ty0)); ("y", PVar (b_id, ty0))]
        ; br_body = Prim (AddInt, [Var a_id; Var b_id]) } ])
    in
    eval empty_env e = VInt 7)

(* Tuple pattern *)
let test_tuple_pat =
  unit_test ~name:"tuple_pattern_destructure" (fun () ->
    let a_id = fresh "a" in
    let b_id = fresh "b" in
    let e = Case (
      Tuple [Lit (LInt 10); Lit (LInt 32)],
      ty0,
      [ { br_pat  = PTuple [PVar (a_id, ty0); PVar (b_id, ty0)]
        ; br_body = Prim (AddInt, [Var a_id; Var b_id]) } ])
    in
    eval empty_env e = VInt 42)

(* Non-exhaustive case raises EvalError *)
let test_nonexhaustive_case =
  unit_test ~name:"nonexhaustive_case_raises" (fun () ->
    let e = Case (Lit (LBool true), ty0,
      [ { br_pat = PLit (LBool false); br_body = Lit (LInt 0) } ]) in
    match eval empty_env e with
    | _ -> false
    | exception EvalError _ -> true)

(* ── Effect handlers ─────────────────────────────────────────────────────── *)

(* HReturn: transforms the return value of the handled computation.
   handle (return 2) with return x → x * 10  should yield 20. *)
let test_hreturn =
  unit_test ~name:"HReturn_transforms_result" (fun () ->
    let x_id = fresh "x" in
    let e = handle (Lit (LInt 2))
      [ HReturn ((x_id, ty0),
          Prim (MulInt, [Var x_id; Lit (LInt 10)])) ]
    in
    eval empty_env e = VInt 20)

(* HReturn is the identity when the clause rebinds and returns unchanged. *)
let test_hreturn_identity =
  Test.make ~name:"HReturn_identity_is_transparent"
    Gen.int_small
    (fun n ->
      let x_id = fresh "x" in
      let e = handle (Lit (LInt n))
        [ HReturn ((x_id, ty0), Var x_id) ]
      in
      eval empty_env e = VInt n)

(* HCtl: non-resumable abort.
   Performing "Fail" under a HCtl handler that returns a constant. *)
let test_hctl_abort =
  unit_test ~name:"HCtl_aborts_to_handler" (fun () ->
    let _x_id = fresh "_x" in
    let e = handle (Perform ("Fail", [Lit LUnit]))
      [ HCtl ("Fail", (_x_id, ty0), Lit (LInt 99)) ]
    in
    eval empty_env e = VInt 99)

(* HCtl: computation after the perform point is NOT reached (no resume). *)
let test_hctl_discards_continuation =
  unit_test ~name:"HCtl_discards_rest_of_computation" (fun () ->
    (* The body would return 999 if it continued past Perform, but HCtl
       intercepts and returns 1 without resuming. *)
    let _x_id = fresh "_x" in
    let e = handle
      (Let (fresh "_", ty0,
        Perform ("Stop", [Lit LUnit]),
        Lit (LInt 999)))
      [ HCtl ("Stop", (_x_id, ty0), Lit (LInt 1)) ]
    in
    eval empty_env e = VInt 1)

(* HOp: resumable operation.
   handle
     let x = perform Ask () in x + 1
   with
     op Ask () k → k 42
   should return 43. *)
let test_hop_resume =
  unit_test ~name:"HOp_resumes_continuation" (fun () ->
    let x_id  = fresh "x" in
    let _u_id = fresh "_u" in
    let k_id  = fresh "k" in
    let body  =
      Let (x_id, ty0,
        Perform ("Ask", [Lit LUnit]),
        Prim (AddInt, [Var x_id; Lit (LInt 1)]))
    in
    let e = handle body
      [ HOp ("Ask", (_u_id, ty0), (k_id, ty0),
          App (Var k_id, [Lit (LInt 42)])) ]
    in
    eval empty_env e = VInt 43)

(* HOp: the same handler fires twice when the operation is performed twice.
   handle
     let x = perform Ask () in
     let y = perform Ask () in
     x + y
   with
     op Ask () k → k 42
   should return 84. *)
let test_hop_resume_twice =
  unit_test ~name:"HOp_resumes_twice" (fun () ->
    let x_id  = fresh "x" in
    let y_id  = fresh "y" in
    let _u_id = fresh "_u" in
    let k_id  = fresh "k" in
    let body =
      Let (x_id, ty0, Perform ("Ask", [Lit LUnit]),
      Let (y_id, ty0, Perform ("Ask", [Lit LUnit]),
        Prim (AddInt, [Var x_id; Var y_id])))
    in
    let e = handle body
      [ HOp ("Ask", (_u_id, ty0), (k_id, ty0),
          App (Var k_id, [Lit (LInt 42)])) ]
    in
    eval empty_env e = VInt 84)

(* HOp: the value passed to the continuation becomes the result of Perform.
   Randomised: handler provides a random n; result should be n + 1. *)
let test_hop_resume_value =
  Test.make ~name:"HOp_continuation_receives_provided_value"
    Gen.int_small
    (fun n ->
      let x_id  = fresh "x" in
      let _u_id = fresh "_u" in
      let k_id  = fresh "k" in
      let body =
        Let (x_id, ty0,
          Perform ("Get", [Lit LUnit]),
          Prim (AddInt, [Var x_id; Lit (LInt 1)]))
      in
      let e = handle body
        [ HOp ("Get", (_u_id, ty0), (k_id, ty0),
            App (Var k_id, [Lit (LInt n)])) ]
      in
      eval empty_env e = VInt (n + 1))

(* HReturn + HOp together: transform after resume.
   handle
     let x = perform Ask () in x
   with
     op Ask () k → k 5
     return v    → v * 2
   HOp fires first (resumes with 5), then HReturn fires on the 5.
   Result: 10. *)
let test_hreturn_and_hop =
  unit_test ~name:"HReturn_applies_after_HOp_resume" (fun () ->
    let x_id  = fresh "x" in
    let _u_id = fresh "_u" in
    let k_id  = fresh "k" in
    let v_id  = fresh "v" in
    let body = Let (x_id, ty0, Perform ("Ask", [Lit LUnit]), Var x_id) in
    let e = handle body
      [ HOp    ("Ask", (_u_id, ty0), (k_id, ty0),
          App (Var k_id, [Lit (LInt 5)]))
      ; HReturn ((v_id, ty0),
          Prim (MulInt, [Var v_id; Lit (LInt 2)])) ]
    in
    eval empty_env e = VInt 10)

(* Unhandled effect propagates as an uncaught OCaml effect (raises). *)
let test_unhandled_effect =
  unit_test ~name:"unhandled_effect_raises" (fun () ->
    let e = Perform ("Unhandled", [Lit LUnit]) in
    match eval empty_env e with
    | _ -> false
    | exception _ -> true)

(* ── generated property tests ───────────────────────────────────────────── *)
(* All use Gen_ir.gen_typed_closed / gen_closed_*.
   Generated programs are well-typed, so EvalError on a generated expr
   is a generator or evaluator bug, not expected behaviour.              *)

let safe_eval e = try Ok (eval empty_env e) with EvalError s -> Error s

(** eval produces only EvalError, never an unexpected OCaml exception. *)
let test_gen_no_exception =
  Test.make ~name:"gen_eval_no_ocaml_exception" ~count:500
    Gen_ir.gen_typed_closed
    (fun (_, e) ->
      try ignore (eval empty_env e); true
      with EvalError _ -> true)

(** Same expression always evaluates to the same result. *)
let test_gen_deterministic =
  Test.make ~name:"gen_eval_deterministic" ~count:500
    Gen_ir.gen_typed_closed
    (fun (_, e) -> safe_eval e = safe_eval e)

(** Ann(e, t) is transparent — never changes the result. *)
let test_gen_ann_transparent =
  Test.make ~name:"gen_ann_transparent" ~count:500
    Gen_ir.gen_typed_closed
    (fun (ty, e) -> safe_eval e = safe_eval (Ann (e, ty)))

(** let x = e in x  ≡  e. *)
let test_gen_let_identity =
  Test.make ~name:"gen_let_identity" ~count:500
    Gen_ir.gen_typed_closed
    (fun (ty, e) ->
      let x = fresh "x" in
      safe_eval e = safe_eval (Let (x, ty, e, Var x)))

(** (fun x → x) e  ≡  e — beta identity. *)
let test_gen_beta_identity =
  Test.make ~name:"gen_beta_identity" ~count:500
    Gen_ir.gen_typed_closed
    (fun (ty, e) ->
      let x = fresh "x" in
      safe_eval e = safe_eval (App (Lam ([(x, ty)], Var x), [e])))

(** ProjI(Tuple [e1; e2], 1) = e1 and ProjI(Tuple [e1; e2], 2) = e2. *)
let test_gen_tuple_proj =
  Test.make ~name:"gen_tuple_proj_roundtrip" ~count:500
    Gen.(pair Gen_ir.gen_typed_closed Gen_ir.gen_typed_closed)
    (fun ((_, e1), (_, e2)) ->
      let tup = Tuple [e1; e2] in
      safe_eval e1 = safe_eval (ProjI (tup, 1)) &&
      safe_eval e2 = safe_eval (ProjI (tup, 2)))

(** AddInt is commutative over generated Int expressions. *)
let test_gen_addint_commutes =
  Test.make ~name:"gen_addint_commutes" ~count:500
    Gen.(pair Gen_ir.gen_closed_int Gen_ir.gen_closed_int)
    (fun (e1, e2) ->
      safe_eval (Prim (AddInt, [e1; e2])) =
      safe_eval (Prim (AddInt, [e2; e1])))

(** MulInt is commutative over generated Int expressions. *)
let test_gen_mulint_commutes =
  Test.make ~name:"gen_mulint_commutes" ~count:500
    Gen.(pair Gen_ir.gen_closed_int Gen_ir.gen_closed_int)
    (fun (e1, e2) ->
      safe_eval (Prim (MulInt, [e1; e2])) =
      safe_eval (Prim (MulInt, [e2; e1])))

(** BAnd and BOr are commutative. *)
let test_gen_bool_commutes =
  Test.make ~name:"gen_bool_and_or_commute" ~count:500
    Gen.(pair Gen_ir.gen_closed_bool Gen_ir.gen_closed_bool)
    (fun (e1, e2) ->
      safe_eval (Prim (BAnd, [e1; e2])) = safe_eval (Prim (BAnd, [e2; e1])) &&
      safe_eval (Prim (BOr,  [e1; e2])) = safe_eval (Prim (BOr,  [e2; e1])))

(** BNot is its own inverse: ¬¬b = b. *)
let test_gen_bnot_involution =
  Test.make ~name:"gen_bnot_involution" ~count:500
    Gen_ir.gen_closed_bool
    (fun e ->
      safe_eval e = safe_eval (Prim (BNot, [Prim (BNot, [e])])))

(** IntToFloat followed by FloatToInt is the identity for small integers. *)
let test_gen_int_float_roundtrip =
  Test.make ~name:"gen_int_to_float_roundtrip" ~count:500
    Gen_ir.gen_closed_int
    (fun e ->
      safe_eval e =
      safe_eval (Prim (FloatToInt, [Prim (IntToFloat, [e])])))

(** AddFloat is commutative over generated Float expressions.
    Uses exact float arithmetic on ints-as-floats so equality holds. *)
let test_gen_addfloat_commutes =
  Test.make ~name:"gen_addfloat_commutes" ~count:500
    Gen.(pair Gen_ir.gen_closed_float Gen_ir.gen_closed_float)
    (fun (e1, e2) ->
      safe_eval (Prim (AddFloat, [e1; e2])) =
      safe_eval (Prim (AddFloat, [e2; e1])))

(* ── entry point ─────────────────────────────────────────────────────────── *)

let () =
  QCheck_base_runner.run_tests_main
    [ (* regression *)
      test_iadd
    ; test_map_length
    ; test_fold_sum
      (* structural *)
    ; test_ann
    ; test_proj_tuple
    ; test_proj_record
    ; test_closure_capture
    ; test_letrec_fact
    ; test_letrec_mutual
    ; test_nested_con_pat
    ; test_record_pat
    ; test_tuple_pat
    ; test_nonexhaustive_case
      (* effects *)
    ; test_hreturn
    ; test_hreturn_identity
    ; test_hctl_abort
    ; test_hctl_discards_continuation
    ; test_hop_resume
    ; test_hop_resume_twice
    ; test_hop_resume_value
    ; test_hreturn_and_hop
    ; test_unhandled_effect
      (* generated properties *)
    ; test_gen_no_exception
    ; test_gen_deterministic
    ; test_gen_ann_transparent
    ; test_gen_let_identity
    ; test_gen_beta_identity
    ; test_gen_tuple_proj
    ; test_gen_addint_commutes
    ; test_gen_mulint_commutes
    ; test_gen_bool_commutes
    ; test_gen_bnot_involution
    ; test_gen_int_float_roundtrip
    ; test_gen_addfloat_commutes
    ]
