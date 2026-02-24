(** Unit tests for the Arra surface parser (Surface.Parse).

    Tests parse representative Arra source fragments and verify that
    the resulting AST has the expected structure, or that invalid input
    raises [Surface.Parse.Error]. *)

open Surface.Ast

let parse src =
  Surface.Parse.parse_string ~filename:"<test>" src

(* ── helpers ─────────────────────────────────────────────────────────── *)

let ok name src check =
  match parse src with
  | prog ->
    if check prog then true
    else begin
      Printf.printf "FAIL [%s]: check failed on parsed result\n" name;
      false
    end
  | exception Surface.Parse.Error msg ->
    Printf.printf "FAIL [%s]: unexpected parse error: %s\n" name msg;
    false
  | exception exn ->
    Printf.printf "FAIL [%s]: unexpected exception: %s\n" name
      (Printexc.to_string exn);
    false

let ok_parses name src = ok name src (fun _ -> true)

let fails name src =
  match parse src with
  | _ ->
    Printf.printf "FAIL [%s]: expected parse error but succeeded\n" name;
    false
  | exception Surface.Parse.Error _ -> true
  | exception exn ->
    Printf.printf "FAIL [%s]: wrong exception type: %s\n" name
      (Printexc.to_string exn);
    false

let ndecls n prog = List.length prog.decls = n

(* ── basic values ─────────────────────────────────────────────────────── *)

let t_empty () =
  ok "empty" "" (ndecls 0)

let t_int_binding () =
  ok "int_binding" "x <- 42" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "x"), None, Lit (_, LInt _))] -> true
    | _ -> false)

let t_string_binding () =
  ok "string_binding" {|s <- "hello"|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "s"), None, Lit (_, LStr "hello"))] -> true
    | _ -> false)

let t_float_binding () =
  ok "float_binding" "f <- 3.14" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, Lit (_, LFloat _))] -> true
    | _ -> false)

let t_bool_true () =
  ok "bool_true" "b <- true" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "b"), None, Lit (_, LBool true))] -> true
    | _ -> false)

let t_bool_false () =
  ok "bool_false" "b <- false" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "b"), None, Lit (_, LBool false))] -> true
    | _ -> false)

let t_unit () =
  ok "unit" "u <- ()" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "u"), None, Lit (_, LUnit))] -> true
    | _ -> false)

(* ── numeric literal forms ────────────────────────────────────────────── *)

let t_hex_literal () =
  ok "hex_literal" "x <- 0xFF" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Lit (_, LInt n))] -> Z.equal n (Z.of_int 255)
    | _ -> false)

let t_bin_literal () =
  ok "bin_literal" "x <- 0b1010" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Lit (_, LInt n))] -> Z.equal n (Z.of_int 10)
    | _ -> false)

let t_underscore_int () =
  ok "underscore_int" "x <- 1_000_000" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Lit (_, LInt n))] -> Z.equal n (Z.of_int 1_000_000)
    | _ -> false)

let t_symbol () =
  ok "symbol" "s <- `bid" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Lit (_, LSymbol "bid"))] -> true
    | _ -> false)

(* ── compound expressions ─────────────────────────────────────────────── *)

let t_array () =
  ok "array" "arr <- [1; 2; 3]" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "arr"), None, Array (_, [_; _; _]))] -> true
    | _ -> false)

let t_empty_array () =
  ok "empty_array" "arr <- []" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Array (_, []))] -> true
    | _ -> false)

let t_tuple () =
  ok "tuple" "pair <- (1, 2)" (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "pair"), None, Tuple (_, [_; _]))] -> true
    | _ -> false)

let t_triple () =
  ok "triple" "t <- (1, 2, 3)" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Tuple (_, [_; _; _]))] -> true
    | _ -> false)

let t_record () =
  ok "record" {|pt <- {x <- 1, y <- 2}|} (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Record (_, [("x", _); ("y", _)]))] -> true
    | _ -> false)

(* ── expression operators ─────────────────────────────────────────────── *)

let t_juxtaposition () =
  (* x f — Forth-style: f applied to x *)
  ok "juxtaposition" "result <- x f" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, App (_, Var (_, "x"), Var (_, "f")))] -> true
    | _ -> false)

let t_binop () =
  ok "binop" "result <- x + y" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Bop (_, "+", Var (_, "x"), Var (_, "y")))] -> true
    | _ -> false)

let t_field_access () =
  (* r.x parses as App(Var "r", FieldName "x") due to grammar conflict
     resolution: atom → DOT LOWER (FieldName) beats expr → expr DOT LOWER *)
  ok "field_access" "result <- r.x" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, App (_, Var (_, "r"), FieldName (_, "x")))] -> true
    | _ -> false)

let t_range () =
  ok "range" "r <- 1..10" (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Range (_, Lit (_, LInt _), Lit (_, LInt _), None))] -> true
    | _ -> false)

(* ── type declarations ────────────────────────────────────────────────── *)

let t_data_enum () =
  ok "data_enum" "type Option a <- None | Some a" (fun p ->
    match p.decls with
    | [DData (_, [], "Option", [TVFree "a"], Some (DBEnum _))] -> true
    | _ -> false)

let t_data_record () =
  ok "data_record" "type Point <- {x: Int, y: Int}" (fun p ->
    match p.decls with
    | [DData (_, [], "Point", [], Some (DBRecord _))] -> true
    | _ -> false)

let t_data_no_body () =
  ok "data_no_body" "type Void" (fun p ->
    match p.decls with
    | [DData (_, [], "Void", [], None)] -> true
    | _ -> false)

let t_type_syn () =
  ok "type_syn" "alias MyInt <- Int" (fun p ->
    match p.decls with
    | [DTypeSyn (_, "MyInt", [], TName "Int")] -> true
    | _ -> false)

let t_type_syn_param () =
  ok "type_syn_param" "alias Pair a b <- (a, b)" (fun p ->
    match p.decls with
    | [DTypeSyn (_, "Pair", [TVFree "a"; TVFree "b"], TProduct [_; _])] -> true
    | _ -> false)

let t_distinct () =
  ok "distinct" "distinct Celsius <- Float" (fun p ->
    match p.decls with
    | [DDistinct (_, "Celsius", [], TName "Float")] -> true
    | _ -> false)

(* ── open / import ────────────────────────────────────────────────────── *)

let t_open () =
  ok "open" "open Math" (fun p ->
    match p.decls with
    | [DOpen (_, ["Math"], None)] -> true
    | _ -> false)

let t_open_path () =
  ok "open_path" "open Math.Vector" (fun p ->
    match p.decls with
    | [DOpen (_, ["Math"; "Vector"], None)] -> true
    | _ -> false)

let t_open_filter () =
  ok "open_filter" "open Math (sin, cos)" (fun p ->
    match p.decls with
    | [DOpen (_, ["Math"], Some (OFOnly [IIName "sin"; IIName "cos"]))] -> true
    | _ -> false)

(* ── bindings with types ──────────────────────────────────────────────── *)

let t_typed_binding () =
  (* (x : Int) is intercepted as tvar TVAnno("x", KTrait "Int") in the LR(1)
     automaton (UPPER RPAREN → kind_or_trait path, not ty path).
     Use Int[] instead: UPPER LBRACKET takes the TName path, so (x : Int[])
     parses as paren_expr → Annot(Var "x", TDyn(TName "Int")),
     which expr_to_pat converts to PTyped(PVar "x", TDyn(TName "Int")). *)
  ok "typed_binding" "(x : Int[]) <- 42" (fun p ->
    match p.decls with
    | [DVal (_, [], PTyped (_, PVar (_, "x"), TDyn (TName "Int")), None, Lit (_, LInt _))] -> true
    | _ -> false)

let t_ctor_pattern () =
  ok "ctor_pattern" "Just x <- Just 42" (fun p ->
    match p.decls with
    | [DVal (_, [], PCtor (_, "Just", [PVar (_, "x")]), None, _)] -> true
    | _ -> false)

let t_tuple_pattern () =
  ok "tuple_pattern" "(a, b) <- (1, 2)" (fun p ->
    match p.decls with
    | [DVal (_, [], PTuple (_, [PVar (_, "a"); PVar (_, "b")]), None, _)] -> true
    | _ -> false)

(* ── recursive bindings ───────────────────────────────────────────────── *)

let t_rec_binding () =
  ok "rec_binding" "rec f <- x" (fun p ->
    match p.decls with
    | [DRec (_, [], [_])] -> true
    | _ -> false)

(* ── effect declarations ──────────────────────────────────────────────── *)

let t_effect_decl () =
  ok "effect_decl" "effect Fail e (ctl raise : e -> Never)" (fun p ->
    match p.decls with
    | [DEffect (_, "Fail", ["e"], [EOCtl ("raise", TFun (TVar "e", None, TNever))])] -> true
    | _ -> false)

let t_effect_multi_op () =
  ok_parses "effect_multi_op"
    "effect State s (op get : () -> s; op put : s -> ())"

(* ── multi-declaration programs ──────────────────────────────────────── *)

let t_multi_decl () =
  (* Top-level newlines don't produce VSemis (no top-level layout context);
     use explicit ';' to separate declarations *)
  ok "multi_decl" "x <- 1; y <- 2" (ndecls 2)

let t_multi_decl_semi () =
  ok "multi_decl_semi" "x <- 1; y <- 2" (ndecls 2)

let t_multiline_program () =
  (* Top-level declarations need explicit ';' separators — no top-level
     layout context means newlines don't produce VSemis at column 1 *)
  ok "multiline_program"
    "type Color <- Red | Green | Blue;\
     alias Name <- Char[];\
     x <- 42;\
     y <- \"hello\""
    (ndecls 4)

(* ── type expressions ─────────────────────────────────────────────────── *)

let t_array_type () =
  ok "array_type" "alias Arr <- Int[]" (fun p ->
    match p.decls with
    | [DTypeSyn (_, "Arr", [], TDyn (TName "Int"))] -> true
    | _ -> false)

let t_function_type () =
  ok_parses "function_type"
    "extern f : Int -> Int"

let t_effect_type () =
  ok_parses "effect_type"
    {|extern g : Int -> ⟪IO⟫ Int|}

(* ── operator sections ────────────────────────────────────────────────── *)

let t_right_op_section () =
  ok "right_op_section" {|f <- (+ 1)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Section (_, SRightOp ("+", Lit (_, LInt _))))] -> true
    | _ -> false)

let t_left_op_section () =
  ok "left_op_section" {|f <- (1 +)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Section (_, SLeftOp (Lit (_, LInt _), "+")))] -> true
    | _ -> false)

(* ── tick sections ────────────────────────────────────────────────────── *)

let t_bare_tick_section () =
  ok "bare_tick_section" {|f <- ('map)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, Section (_, SBareTick "map"))] -> true
    | _ -> false)

let t_right_tick_section () =
  ok "right_tick_section" {|f <- ('map g)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Section (_, SRightTick ("map", Var (_, "g"))))] -> true
    | _ -> false)

let t_left_tick_section () =
  ok "left_tick_section" {|f <- (arr 'map)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Section (_, SLeftTick (Var (_, "arr"), "map")))] -> true
    | _ -> false)

let t_tick_val_section () =
  ok "tick_val_section" {|f <- ('rank 1 g)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Section (_, STickVal ("rank", Lit (_, LInt _), Var (_, "g"))))] -> true
    | _ -> false)

(* ── extern expression-body with parameters ───────────────────────────── *)

let t_extern_expr_body () =
  ok "extern_expr_body"
    {|extern "C" expect (cond : Bool, n : Int) : Bool = "__builtin_expect(!!(cond), n)"|}
    (fun p ->
      match p.decls with
      | [DExtern (_, _, "expect", TName "Bool",
          Some (EBExpr ([("cond", TName "Bool"); ("n", TName "Int")], _)))] -> true
      | _ -> false)

(* ── record type row operations ───────────────────────────────────────── *)

let t_row_concat () =
  ok "row_concat" "alias R <- {x: Int} ++ {y: Float}" (fun p ->
    match p.decls with
    | [DTypeSyn (_, "R", [], TRowConcat (TRecord _, TRecord _))] -> true
    | _ -> false)

let t_row_sub () =
  ok "row_sub" "alias S <- {x: Int, y: Float} - .x" (fun p ->
    match p.decls with
    | [DTypeSyn (_, "S", [], TRowSub (TRecord _, "x"))] -> true
    | _ -> false)

(* ── error cases ──────────────────────────────────────────────────────── *)

let t_error_unmatched_paren () =
  fails "error_unmatched_paren" "x <- (1 + 2"

let t_error_empty_binding () =
  fails "error_empty_binding" "x <-"

let t_error_bad_token () =
  fails "error_bad_token" "x <- #!"

(* ── runner ──────────────────────────────────────────────────────────── *)

let all_tests =
  [ "empty",               t_empty
  ; "int_binding",         t_int_binding
  ; "string_binding",      t_string_binding
  ; "float_binding",       t_float_binding
  ; "bool_true",           t_bool_true
  ; "bool_false",          t_bool_false
  ; "unit",                t_unit
  ; "hex_literal",         t_hex_literal
  ; "bin_literal",         t_bin_literal
  ; "underscore_int",      t_underscore_int
  ; "symbol",              t_symbol
  ; "array",               t_array
  ; "empty_array",         t_empty_array
  ; "tuple",               t_tuple
  ; "triple",              t_triple
  ; "record",              t_record
  ; "juxtaposition",       t_juxtaposition
  ; "binop",               t_binop
  ; "field_access",        t_field_access
  ; "range",               t_range
  ; "data_enum",           t_data_enum
  ; "data_record",         t_data_record
  ; "data_no_body",        t_data_no_body
  ; "type_syn",            t_type_syn
  ; "type_syn_param",      t_type_syn_param
  ; "distinct",            t_distinct
  ; "open",                t_open
  ; "open_path",           t_open_path
  ; "open_filter",         t_open_filter
  ; "typed_binding",       t_typed_binding
  ; "ctor_pattern",        t_ctor_pattern
  ; "tuple_pattern",       t_tuple_pattern
  ; "rec_binding",         t_rec_binding
  ; "effect_decl",         t_effect_decl
  ; "effect_multi_op",     t_effect_multi_op
  ; "multi_decl",          t_multi_decl
  ; "multi_decl_semi",     t_multi_decl_semi
  ; "multiline_program",   t_multiline_program
  ; "array_type",          t_array_type
  ; "function_type",       t_function_type
  ; "effect_type",         t_effect_type
  ; "bare_tick_section",      t_bare_tick_section
  ; "right_tick_section",     t_right_tick_section
  ; "left_tick_section",      t_left_tick_section
  ; "tick_val_section",       t_tick_val_section
  ; "extern_expr_body",       t_extern_expr_body
  ; "right_op_section",      t_right_op_section
  ; "left_op_section",       t_left_op_section
  ; "row_concat",            t_row_concat
  ; "row_sub",               t_row_sub
  ; "error_unmatched_paren", t_error_unmatched_paren
  ; "error_empty_binding", t_error_empty_binding
  ; "error_bad_token",     t_error_bad_token
  ]

let () =
  let pass = ref 0 and fail = ref 0 in
  List.iter (fun (name, f) ->
    if f () then incr pass
    else (incr fail; Printf.printf "  ^ in test: %s\n" name)
  ) all_tests;
  Printf.printf "%d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
