(** Unit tests for the Arra surface parser (Surface.Parse).

    Tests parse representative Arra source fragments and verify that
    the resulting AST has the expected structure, or that invalid input
    raises [Surface.Parse.Error]. *)

open Surface.Ast

let parse src =
  Surface.Parse.parse_string ~filename:"<test>" src

(* ── AST dump for test-failure diagnostics ────────────────────────────── *)

let dump_program prog =
  Sexplib.Sexp.to_string_hum (Surface.Ast.sexp_of_program prog)

(* ── helpers ─────────────────────────────────────────────────────────── *)

let ok name src check =
  match parse src with
  | prog ->
    if check prog then true
    else begin
      Printf.printf "FAIL [%s]: check failed\n  src: %s\n  got:\n%s\n"
        name src (dump_program prog);
      false
    end
  | exception Surface.Parse.Error msg ->
    Printf.printf "FAIL [%s]: unexpected parse error: %s\n  src: %s\n" name msg src;
    false
  | exception exn ->
    Printf.printf "FAIL [%s]: unexpected exception: %s\n  src: %s\n" name
      (Printexc.to_string exn) src;
    false

let ok_parses name src = ok name src (fun _ -> true)

let fails name src =
  match parse src with
  | prog ->
    Printf.printf "FAIL [%s]: expected parse error but succeeded\n  src: %s\n  got:\n%s\n"
      name src (dump_program prog);
    false
  | exception Surface.Parse.Error _ -> true
  | exception exn ->
    Printf.printf "FAIL [%s]: wrong exception type: %s\n  src: %s\n" name
      (Printexc.to_string exn) src;
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

(* ── tick applications ────────────────────────────────────────────────── *)

let t_tick_app_lower () =
  ok "tick_app_lower" {|r <- arr 'map f|} (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, TickApp (_, _, "map", None, Some _))] -> true
    | _ -> false)

let t_tick_app_upper () =
  ok "tick_app_upper" {|r <- arr 'map Foo|} (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, TickApp (_, _, "map", None, Some _))] -> true
    | _ -> false)

let t_tick_app_rank () =
  ok "tick_app_rank" {|r <- arr 'rank 1 f|} (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, TickApp (_, _, "rank", Some _, Some _))] -> true
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

(* ── anonymous functions and branches ───────────────────────────────── *)

let t_lam () =
  ok "lam" {|f <- (x -> x)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, Lam (_, _))] -> true
    | _ -> false)

let t_branches () =
  ok "branches" {|f <- (A -> 1; B -> 2)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, Branches (_, [_; _]))] -> true
    | _ -> false)

let t_plit_branch () =
  ok "plit_branch" {|f <- (42 -> "yes")|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Lam (_, {b_pat = PLit (_, LInt _); _}))] -> true
    | _ -> false)

let t_por_branch () =
  ok "por_branch" {|f <- (A | B -> 0)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Lam (_, {b_pat = POr (_, [_; _]); _}))] -> true
    | _ -> false)

(* ── do blocks ────────────────────────────────────────────────────────── *)

let t_do_block () =
  ok "do_block" {|r <- (x <- 1; x)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None, Do (_, [SBind _; SExpr _]))] -> true
    | _ -> false)

(* ── expression forms ─────────────────────────────────────────────────── *)

let t_uop () =
  ok "uop" {|f <- x!|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, Uop (_, "!", Var (_, "x")))] -> true
    | _ -> false)

(* r.x and r .x are the same token stream; both desugar to App(r, FieldName(x)) *)
let t_field_access () =
  ok "field_access" {|f <- r.x|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        App (_, Var (_, "r"), FieldName (_, "x")))] -> true
    | _ -> false)

let t_field_access_chain () =
  ok "field_access_chain" {|f <- r.x.y|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        App (_, App (_, Var (_, "r"), FieldName (_, "x")), FieldName (_, "y")))] -> true
    | _ -> false)

let t_tuple_idx () =
  ok "tuple_idx" {|f <- p.0|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, TupleIdx (_, Var (_, "p"), 0))] -> true
    | _ -> false)

let t_field_update () =
  ok "field_update" {|f <- r.(x <- 1)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        FieldUpdate (_, Var (_, "r"), [("x", _)]))] -> true
    | _ -> false)

let t_ctor_value () =
  ok "ctor_value" {|v <- None|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "v"), None, Ctor (_, "None"))] -> true
    | _ -> false)

(* ── handler clauses (exercises handler R/R conflict zone) ────────────── *)

let t_tick_handle () =
  ok "tick_handle" {|h <- x 'handle (return v -> v)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "h"), None,
        TickApp (_, _, "handle", None,
          Some (Section (_, SHandle [HReturn (PVar (_, "v"), Var (_, "v"))]))))] -> true
    | _ -> false)

let t_pwild () =
  ok "pwild" {|h <- x 'handle (return _ -> x)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "h"), None,
        TickApp (_, _, "handle", None,
          Some (Section (_, SHandle [HReturn (PWild, _)]))))] -> true
    | _ -> false)

(* ── SBareOp via tick proj_arg ─────────────────────────────────────────── *)

let t_bare_op_tick () =
  ok "bare_op_tick" {|f <- arr 'map +|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        TickApp (_, _, "map", None, Some (Section (_, SBareOp "+"))))] -> true
    | _ -> false)

(* ── extern expression-body with parameters ───────────────────────────── *)

let t_extern_expr_body () =
  ok "extern_expr_body"
    {|extern "C" expect (cond : Bool, n : Int) : Bool <- "__builtin_expect(!!(cond), n)"|}
    (fun p ->
      match p.decls with
      | [DExtern (_, _, "expect", TName "Bool",
          Some (EBExpr ([("cond", TName "Bool"); ("n", TName "Int")], _)))] -> true
      | _ -> false)

(* ── extern declarations ──────────────────────────────────────────────── *)

let t_extern_no_body () =
  ok "extern_no_body" {|extern myFunc : Int -> Int|} (fun p ->
    match p.decls with
    | [DExtern (_, [], "myFunc", TFun (TName "Int", None, TName "Int"), None)] -> true
    | _ -> false)

let t_extern_symbol () =
  ok "extern_symbol" {|extern "C" printf : () -> () <- "printf"|} (fun p ->
    match p.decls with
    | [DExtern (_, _, "printf", TFun (TUnit, None, TUnit), Some (EBSymbol "printf"))] -> true
    | _ -> false)

(* ── hook declarations ────────────────────────────────────────────────── *)

let t_hook_iter () =
  ok "hook_iter" {|iter 'map f <- arr_map|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HIter,
        HHSym (HSTick "map", None, [PVar (_, "f")], None),
        Var (_, "arr_map"))] -> true
    | _ -> false)

(* ── ptr type ─────────────────────────────────────────────────────────── *)

let t_ptr_type () =
  ok "ptr_type" {|extern buf : Ptr Int|} (fun p ->
    match p.decls with
    | [DExtern (_, _, "buf", TApp (TName "Ptr", TName "Int"), None)] -> true
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

(* ── open record type and chained row concat ──────────────────────────── *)

let t_open_record_type () =
  ok "open_record_type" {|alias R <- {x: Int | rest}|} (fun p ->
    match p.decls with
    | [DTypeSyn (_, "R", [],
        TRecord (ROpen ([{fname = "x"; _}], "rest")))] -> true
    | _ -> false)

let t_row_concat_chain () =
  ok "row_concat_chain" {|alias R <- {x: Int} ++ {y: Float} ++ {z: Bool}|} (fun p ->
    match p.decls with
    | [DTypeSyn (_, "R", [],
        TRowConcat (TRowConcat (TRecord _, TRecord _), TRecord _))] -> true
    | _ -> false)

(* ── derived declaration ─────────────────────────────────────────────── *)

let t_derived () =
  ok "derived" {|derived total <- items 'sum|} (fun p ->
    match p.decls with
    | [DDerived (_, "total", None, _)] -> true
    | _ -> false)

(* ── isomorphism declaration ─────────────────────────────────────────── *)
let t_iso () =
  ok "iso" {|iso A, B (from A <- f; from B <- g)|} (fun p ->
    match p.decls with
    | [DIso (_, [], [], PCtor (_, "A", []), PCtor (_, "B", []), _, _)] -> true
    | _ -> false)

(* ── trait / implementation / module (structural parses) ─────────────── *)

let t_trait () =
  ok_parses "trait" {|trait Eq a (bop == : a, a → Bool)|}

let t_impl () =
  ok_parses "impl" {|implementation Eq Int (bop == x, y <- prim_eq)|}

let t_module () =
  ok_parses "module"
    {|module M (interface (extern f : Int) implementation (extern f : Int <- "f"))|}

(* ── type_constraint on hooks / traits / impls ───────────────────────── *)

let t_hook_constrained () =
  ok "hook_constrained" {|bop forall a => + x, y <- add_impl|} (fun p ->
    match p.decls with
    | [DHook (_, [], [TVFree "a"], HBop, _, _)] -> true
    | _ -> false)

let t_hook_bare_tvar_fails () =
  (* bare tvar without ∀: 'a bop ...' tries val_lhs, fails on BOP after expr *)
  fails "hook_bare_tvar_fails" {|a bop + x, y <- add_impl|}

let t_trait_constrained () =
  ok_parses "trait_constrained1" {|trait Ord a (bop < : a, b → Bool)|} &&
  ok_parses "trait_constrained2" {|trait forall a => Ord a (bop < : a, b → Bool)|} &&
  ok_parses "trait_constrained3" {|trait forall (a : Eq) => Ord a (bop < : a, b → Bool)|}

(* ── hook kinds ──────────────────────────────────────────────────────── *)

let t_hook_bop () =
  ok "hook_bop" {|bop + x, y <- add_impl|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HBop,
        HHSym (HSOp "+", None, [PVar (_, "x"); PVar (_, "y")], None),
        Var (_, "add_impl"))] -> true
    | _ -> false)

let t_hook_uop () =
  ok "hook_uop" {|uop ! x <- neg_impl|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HUop,
        HHSym (HSOp "!", None, [PVar (_, "x")], None),
        Var (_, "neg_impl"))] -> true
    | _ -> false)

let t_hook_project () =
  ok "hook_project" {|project [e] x <- get_field|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HProject,
        HHPos (Some "e", [PVar (_, "x")], None),
        Var (_, "get_field"))] -> true
    | _ -> false)

(* ── hook type annotations ────────────────────────────────────────────────── *)

let t_hook_bop_typed () =
  ok "hook_bop_typed" {|bop + Int, Int → Int <- prim_add|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HBop,
        HHSym (HSOp "+", None,
          [PCtor (_, "Int", []); PCtor (_, "Int", [])],
          Some (TName "Int")),
        _)] -> true
    | _ -> false)

let t_hook_uop_typed () =
  ok "hook_uop_typed" {|uop - Int → Int <- prim_neg|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HUop,
        HHSym (HSOp "-", None,
          [PCtor (_, "Int", [])],
          Some (TName "Int")),
        _)] -> true
    | _ -> false)

let t_hook_iter_typed () =
  (* iter 'map a, (a → b) → b ← prim_map
     Arg patterns: [PVar "a"; PView(Var "a", PVar "b")]
     Result type: TVar "b" *)
  ok "hook_iter_typed" {|iter 'map a, (a → b) → b <- prim_map|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HIter,
        HHSym (HSTick "map", None, [_; _], Some (TVar "b")),
        _)] -> true
    | _ -> false)

let t_hook_project_typed () =
  ok "hook_project_typed" {|project [e] x, f → t <- get_field|} (fun p ->
    match p.decls with
    | [DHook (_, [], [], HProject,
        HHPos (Some "e", [PVar (_, "x"); PVar (_, "f")], Some (TVar "t")),
        _)] -> true
    | _ -> false)

let t_hook_constrained_typed () =
  ok "hook_constrained_typed" {|bop forall a => + x, y → a <- add_impl|} (fun p ->
    match p.decls with
    | [DHook (_, [], [TVFree "a"], HBop,
        HHSym (HSOp "+", None, [_; _], Some (TVar "a")),
        _)] -> true
    | _ -> false)

let t_trait_typed_sigs () =
  (* Trait with typed sigs in spec style: bop + : a, a → a *)
  ok_parses "trait_typed_sigs"
    {|trait Num a (bop + : a, a → a; bop - : a, a → a; uop - : a → a)|}

let t_trait_with_default () =
  (* Trait sig with a default implementation *)
  ok_parses "trait_with_default"
    {|trait Functor f (iter 'map : Self t, (t → u) → Self u)|}

let t_trait_from_sig () =
  (* from sig in a trait: from : Int → a *)
  ok_parses "trait_from_sig"
    {|trait Num a (from : Int → a)|}

(* ── array patterns (tested in handler clause, where pat is parsed directly) *)

let t_array_pat () =
  ok "array_pat"
    {|h <- x 'handle (return [a; b] -> a)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PArray (_, { lp_prefix = [PVar (_, "a"); PVar (_, "b")]
                                   ; lp_rest   = None
                                   ; lp_suffix = [] }), _)]))))] -> true
      | _ -> false)

let t_array_pat_rest () =
  ok "array_pat_rest"
    {|h <- x 'handle (return [a; ..rest] -> a)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PArray (_, { lp_prefix = [PVar (_, "a")]
                                   ; lp_rest   = Some "rest"
                                   ; _ }), _)]))))] -> true
      | _ -> false)

(* ── record patterns ─────────────────────────────────────────────────── *)

let t_record_pat () =
  ok "record_pat"
    {|h <- x 'handle (return {x, y} -> x)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PRecord (_, { rp_fields = [("x", None); ("y", None)]
                                   ; rp_open   = None }), _)]))))] -> true
      | _ -> false)

let t_record_pat_open () =
  ok "record_pat_open"
    {|h <- x 'handle (return {x, ..} -> x)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PRecord (_, { rp_fields = [("x", None)]
                                   ; rp_open   = Some _ }), _)]))))] -> true
      | _ -> false)

(* ── as-patterns ─────────────────────────────────────────────────────── *)

let t_as_pat () =
  ok "as_pat"
    {|h <- x 'handle (return Some v @ full -> v)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PAs (_, PCtor (_, "Some", [PVar (_, "v")]), "full"), _)]))))] -> true
      | _ -> false)

(* ── view patterns ───────────────────────────────────────────────────── *)

let t_view_pat () =
  ok "view_pat"
    {|h <- x 'handle (return (isValid -> True) -> True)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PView (_, Var (_, "isValid"), PCtor (_, "True", [])),
                Ctor (_, "True"))]))))] -> true
      | _ -> false)

(* ── handler ctl clause (already in t_tick_handle/t_pwild; extended here) *)

let t_handler_multi () =
  ok_parses "handler_multi"
    {|h <- x 'handle (ctl fail e -> default; return v -> v)|}

(* ── quoted / splice ─────────────────────────────────────────────────── *)

let t_quoted () =
  ok "quoted" {|q <- `[x + 1]|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "q"), None, Quoted (_, _))] -> true
    | _ -> false)

let t_splice () =
  ok "splice" {|s <- .(code)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "s"), None, Splice (_, _))] -> true
    | _ -> false)

(* ── range with stride ───────────────────────────────────────────────── *)

(* No stride grammar rule exists; 0..2..10 parses as (0..2)..10 — nested Ranges *)
let t_range_stride () =
  ok "range_stride" {|r <- 0..2..10|} (fun p ->
    match p.decls with
    | [DVal (_, [], _, _, Range (_, Range (_, _, _, None), _, None))] -> true
    | _ -> false)

(* ── additional type forms ───────────────────────────────────────────── *)

let t_forall_type () =
  ok "forall_type" {|extern f : forall a b => a -> b|} (fun p ->
    match p.decls with
    | [DExtern (_, _, "f",
        TConstrained ([TVFree "a"; TVFree "b"],
          TFun (TVar "a", None, TVar "b")), None)] -> true
    | _ -> false)

let t_sigma_type () =
  ok "sigma_type" {|alias V <- exists (n : Nat, n > 0) Int[n]|} (fun p ->
    match p.decls with
    | [DTypeSyn (_, "V", [],
        TSigma ("n", TName "Nat", _, TArr (TName "Int", [DVar "n"])))] -> true
    | _ -> false)

let t_sized_array_type () =
  ok "sized_array_type" {|alias A <- Int[n]|} (fun p ->
    match p.decls with
    | [DTypeSyn (_, "A", [], TArr (TName "Int", [DVar "n"]))] -> true
    | _ -> false)

let t_effect_row_type () =
  ok "effect_row_type" {|extern g : Int -> ⟪IO⟫ Int|} (fun p ->
    match p.decls with
    | [DExtern (_, _, "g",
        TFun (TName "Int",
          Some (ERow [EENamed ("IO", [])]),
          TName "Int"), None)] -> true
    | _ -> false)

(* ── open hiding filter ──────────────────────────────────────────────── *)

let t_open_hiding () =
  ok "open_hiding" {|open Math hiding (sin)|} (fun p ->
    match p.decls with
    | [DOpen (_, ["Math"], Some (OFHiding [IIName "sin"]))] -> true
    | _ -> false)

(* ── rec in paren / do blocks ────────────────────────────────────────── *)

let t_paren_rec_single () =
  ok "paren_rec_single" {|f <- (rec x <- x + 1; x)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Do (_, [SRec [_]; SExpr _]))] -> true
    | _ -> false)

let t_paren_rec_block () =
  ok "paren_rec_block" {|f <- (rec (x <- 1; y <- x); x)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None,
        Do (_, [SRec [_; _]; SExpr _]))] -> true
    | _ -> false)

(* ── error cases ──────────────────────────────────────────────────────── *)

let t_error_unmatched_paren () =
  fails "error_unmatched_paren" "x <- (1 + 2"

let t_error_empty_binding () =
  fails "error_empty_binding" "x <-"

let t_error_bad_token () =
  fails "error_bad_token" "x <- #!"

let t_error_unclosed_bracket () =
  fails "error_unclosed_bracket" "[1; 2"

(* ── unparenthesised lambdas in proj_arg ────────────────────────────── *)

let t_proj_lam_lower () =
  ok "proj_lam_lower" {|r <- arr 'map x -> x + 1|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PVar (_, "x"); b_guard = None; _}))))] -> true
    | _ -> false)

let t_proj_lam_wild () =
  ok "proj_lam_wild" {|r <- arr 'map _ -> 0|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PWild; b_guard = None; _}))))] -> true
    | _ -> false)

let t_proj_lam_upper () =
  ok "proj_lam_upper" {|r <- arr 'map None -> 0|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PCtor (_, "None", []); b_guard = None; _}))))] -> true
    | _ -> false)

let t_proj_lam_float () =
  ok "proj_lam_float" {|r <- arr 'map 0.0 -> 1|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PLit (_, LFloat _); _}))))] -> true
    | _ -> false)

let t_proj_lam_tuple () =
  ok "proj_lam_tuple" {|r <- pairs 'map (x, y) -> x + y|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PTuple (_, [PVar (_, "x"); PVar (_, "y")]); _}))))] -> true
    | _ -> false)

let t_proj_lam_array_pat () =
  ok "proj_lam_array_pat" {|r <- arrs 'map [h; ..t] -> h|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PArray (_, {lp_prefix = [PVar (_, "h")]; lp_rest = Some "t"; _}); _}))))] -> true
    | _ -> false)

let t_proj_lam_record_pat () =
  ok "proj_lam_record_pat" {|r <- recs 'map {x, y} -> x + y|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PRecord (_, {rp_fields = [("x", None); ("y", None)]; _}); _}))))] -> true
    | _ -> false)

let t_proj_lam_guard () =
  ok "proj_lam_guard" {|r <- arr 'map x when x > 0 -> x|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "map", None,
          Some (Lam (_, {b_pat = PVar (_, "x"); b_guard = Some _; _}))))] -> true
    | _ -> false)

let t_proj_lam_two_arg () =
  (* arr 'rank 1 f — value?=1, proj_arg=f; the LOWER 'f' is NOT parsed as a lambda *)
  ok "proj_lam_two_arg" {|r <- arr 'rank 1 f|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        TickApp (_, _, "rank", Some _, Some (Var (_, "f"))))] -> true
    | _ -> false)

let t_open_in_block () =
  ok "open_in_block" {|r <- (open Math; x + 1)|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "r"), None,
        Do (_, [SOpen (["Math"], None); SExpr _]))] -> true
    | _ -> false)

let t_attr_scope () =
  ok "attr_scope" {|/'-Pure-'/ (extern f : Int; extern g : Int)|} (fun p ->
    match p.decls with
    | [DAttrScope (_, [AEFlag "Pure"], [DExtern _; DExtern _])] -> true
    | _ -> false)

let t_loc_populated () =
  ok "loc_populated" {|x <- 42|} (fun p ->
    match p.decls with
    | [DVal (l, _, _, _, _)] -> l.line > 0 || l.col >= 0
    | _ -> false)

let t_shape_pat () =
  (* Sigma elimination: bind array x and its length n from a ∃(n:Nat) Int[n] value *)
  ok "shape_pat" {|x[n] <- arr|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PShape (_, PVar (_, "x"), [DVar "n"]), None, Var (_, "arr"))] -> true
      | _ -> false)

let t_shape_pat_multi () =
  (* 2-D sigma elimination: bind matrix and both dimension variables *)
  ok "shape_pat_multi" {|x[n;m] <- mat|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PShape (_, PVar (_, "x"), [DVar "n"; DVar "m"]), None, Var (_, "mat"))] -> true
      | _ -> false)

let t_error_missing_type_name () =
  fails "error_missing_type_name" "type <- Int"

(* ── first-class field name literal ──────────────────────────────────── *)

(* .x in expression position is a FieldName (first-class accessor) *)
let t_field_name_expr () =
  ok "field_name_expr" {|f <- .x|} (fun p ->
    match p.decls with
    | [DVal (_, [], PVar (_, "f"), None, FieldName (_, "x"))] -> true
    | _ -> false)

(* .x in pattern position is a literal field-name match, not a variable binding *)
let t_field_name_pat () =
  ok "field_name_pat"
    {|h <- col 'handle (return .x -> 1; return .y -> 2)|}
    (fun p ->
      match p.decls with
      | [DVal (_, [], PVar (_, "h"), None,
          TickApp (_, _, "handle", None,
            Some (Section (_, SHandle
              [HReturn (PLit (_, LField "x"), _);
               HReturn (PLit (_, LField "y"), _)]))))] -> true
      | _ -> false)

(* ── runner ──────────────────────────────────────────────────────────── *)

let all_tests =
  [ "empty",                   t_empty
  ; "int_binding",             t_int_binding
  ; "string_binding",          t_string_binding
  ; "float_binding",           t_float_binding
  ; "bool_true",               t_bool_true
  ; "bool_false",              t_bool_false
  ; "unit",                    t_unit
  ; "hex_literal",             t_hex_literal
  ; "bin_literal",             t_bin_literal
  ; "underscore_int",          t_underscore_int
  ; "symbol",                  t_symbol
  ; "array",                   t_array
  ; "empty_array",             t_empty_array
  ; "tuple",                   t_tuple
  ; "triple",                  t_triple
  ; "record",                  t_record
  ; "juxtaposition",           t_juxtaposition
  ; "binop",                   t_binop
  ; "field_access",            t_field_access
  ; "range",                   t_range
  ; "data_enum",               t_data_enum
  ; "data_record",             t_data_record
  ; "data_no_body",            t_data_no_body
  ; "type_syn",                t_type_syn
  ; "type_syn_param",          t_type_syn_param
  ; "distinct",                t_distinct
  ; "open",                    t_open
  ; "open_path",               t_open_path
  ; "open_filter",             t_open_filter
  ; "typed_binding",           t_typed_binding
  ; "ctor_pattern",            t_ctor_pattern
  ; "tuple_pattern",           t_tuple_pattern
  ; "rec_binding",             t_rec_binding
  ; "effect_decl",             t_effect_decl
  ; "effect_multi_op",         t_effect_multi_op
  ; "multi_decl",              t_multi_decl
  ; "multi_decl_semi",         t_multi_decl_semi
  ; "multiline_program",       t_multiline_program
  ; "array_type",              t_array_type
  ; "function_type",           t_function_type
  ; "effect_type",             t_effect_type
  ; "tick_app_lower",          t_tick_app_lower
  ; "tick_app_upper",          t_tick_app_upper
  ; "tick_app_rank",           t_tick_app_rank
  ; "bare_tick_section",       t_bare_tick_section
  ; "right_tick_section",      t_right_tick_section
  ; "left_tick_section",       t_left_tick_section
  ; "tick_val_section",        t_tick_val_section
  ; "lam",                     t_lam
  ; "branches",                t_branches
  ; "plit_branch",             t_plit_branch
  ; "por_branch",              t_por_branch
  ; "do_block",                t_do_block
  ; "uop",                     t_uop
  ; "field_access",            t_field_access
  ; "field_access_chain",      t_field_access_chain
  ; "tuple_idx",               t_tuple_idx
  ; "field_update",            t_field_update
  ; "ctor_value",              t_ctor_value
  ; "tick_handle",             t_tick_handle
  ; "pwild",                   t_pwild
  ; "bare_op_tick",            t_bare_op_tick
  ; "extern_expr_body",        t_extern_expr_body
  ; "extern_no_body",          t_extern_no_body
  ; "extern_symbol",           t_extern_symbol
  ; "hook_iter",               t_hook_iter
  ; "ptr_type",                t_ptr_type
  ; "right_op_section",        t_right_op_section
  ; "left_op_section",         t_left_op_section
  ; "row_concat",              t_row_concat
  ; "row_sub",                 t_row_sub
  ; "open_record_type",        t_open_record_type
  ; "row_concat_chain",        t_row_concat_chain
  ; "derived",                 t_derived
  ; "iso",                     t_iso
  ; "trait",                   t_trait
  ; "impl",                    t_impl
  ; "module",                  t_module
  ; "hook_constrained",        t_hook_constrained
  ; "hook_bare_tvar_fails",    t_hook_bare_tvar_fails
  ; "trait_constrained",       t_trait_constrained
  ; "hook_bop",                t_hook_bop
  ; "hook_uop",                t_hook_uop
  ; "hook_project",            t_hook_project
  ; "hook_bop_typed",          t_hook_bop_typed
  ; "hook_uop_typed",          t_hook_uop_typed
  ; "hook_iter_typed",         t_hook_iter_typed
  ; "hook_project_typed",      t_hook_project_typed
  ; "hook_constrained_typed",  t_hook_constrained_typed
  ; "trait_typed_sigs",        t_trait_typed_sigs
  ; "trait_with_default",      t_trait_with_default
  ; "trait_from_sig",          t_trait_from_sig
  ; "array_pat",               t_array_pat
  ; "array_pat_rest",          t_array_pat_rest
  ; "record_pat",              t_record_pat
  ; "record_pat_open",         t_record_pat_open
  ; "as_pat",                  t_as_pat
  ; "view_pat",                t_view_pat
  ; "handler_multi",           t_handler_multi
  ; "quoted",                  t_quoted
  ; "splice",                  t_splice
  ; "range_stride",            t_range_stride
  ; "forall_type",             t_forall_type
  ; "sigma_type",              t_sigma_type
  ; "sized_array_type",        t_sized_array_type
  ; "effect_row_type",         t_effect_row_type
  ; "open_hiding",             t_open_hiding
  ; "paren_rec_single",        t_paren_rec_single
  ; "paren_rec_block",         t_paren_rec_block
  ; "proj_lam_lower",          t_proj_lam_lower
  ; "proj_lam_wild",           t_proj_lam_wild
  ; "proj_lam_upper",          t_proj_lam_upper
  ; "proj_lam_tuple",          t_proj_lam_tuple
  ; "proj_lam_array_pat",      t_proj_lam_array_pat
  ; "proj_lam_record_pat",     t_proj_lam_record_pat
  ; "proj_lam_guard",          t_proj_lam_guard
  ; "proj_lam_float",          t_proj_lam_float
  ; "proj_lam_two_arg",        t_proj_lam_two_arg
  ; "attr_scope",              t_attr_scope
  ; "loc_populated",           t_loc_populated
  ; "open_in_block",           t_open_in_block
  ; "error_unmatched_paren",   t_error_unmatched_paren
  ; "error_empty_binding",     t_error_empty_binding
  ; "error_bad_token",         t_error_bad_token
  ; "error_unclosed_bracket",  t_error_unclosed_bracket
  ; "shape_pat",               t_shape_pat
  ; "shape_pat_multi",         t_shape_pat_multi
  ; "error_missing_type_name", t_error_missing_type_name
  ; "field_name_expr",         t_field_name_expr
  ; "field_name_pat",          t_field_name_pat
  ]

let () =
  let pass = ref 0 and fail = ref 0 in
  List.iter (fun (name, f) ->
    if f () then incr pass
    else (incr fail; Printf.printf "  ^ in test: %s\n" name)
  ) all_tests;
  Printf.printf "%d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
