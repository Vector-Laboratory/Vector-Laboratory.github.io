(** Unit tests for the layout preprocessor (Surface.Layout).

    Each test spells out the exact input token stream and expected output
    so that failures pinpoint the precise case that broke.

    See doc/SYNTAX.md §1.7 for the layout rule specification. *)

open Surface.Layout

(* ── helpers ────────────────────────────────────────────────────────────── *)

let t tok line col = { token = tok; pos = { line; col } }
let nm  s l c = t (TName s)   l c
let op  s l c = t (TOp   s)   l c
let lit s l c = t (TLit  s)   l c
let lp    l c = t LParen      l c
let rp    l c = t RParen      l c
let sm    l c = t Semi        l c

let tokens tl = List.map (fun tl -> tl.token) tl

let check_eq name input expected =
  let got = tokens (run input) in
  if got = expected then true
  else begin
    let show = function
      | TName s -> s  | TOp s -> s   | TLit s -> s
      | LParen  -> "(" | RParen  -> ")"
      | LBracket-> "[" | RBracket-> "]"
      | LBrace  -> "{" | RBrace  -> "}"
      | LEffect -> "⟪" | REffect -> "⟫"
      | BacktickLBracket -> "`[" | AttrOpen -> "/'- " | AttrClose -> "-'/"
      | Semi    -> ";" | VSemi   -> "VSEMI" | EOF -> "EOF"
    in
    Printf.printf "FAIL [%s]\n  expected: [%s]\n  got:      [%s]\n" name
      (String.concat " " (List.map show expected))
      (String.concat " " (List.map show got));
    false
  end

(* ── tests ──────────────────────────────────────────────────────────────── *)

(** Empty input produces empty output. *)
let t_empty () =
  check_eq "empty" [] []

(** Single token with no block opener — no VSemi injected. *)
let t_single () =
  check_eq "single_token"
    [ nm "x" 1 1 ]
    [ TName "x" ]

(** Explicit semicolon is passed through unchanged. *)
let t_explicit_semi () =
  check_eq "explicit_semi_passthrough"
    [ nm "a" 1 1; sm 1 3; nm "b" 1 5 ]
    [ TName "a"; Semi; TName "b" ]

(** ─────────────────────────────────────────────────────
    Same-line ( suppression:
      f ← (x + y)
    The ( follows ← on the same line → no layout context opened. *)
let t_same_line_paren_suppression () =
  check_eq "same_line_paren_suppresses_layout"
    [ nm "f"  1 1
    ; op "←"  1 3
    ; lp      1 5   (* same line as ← → suppress *)
    ; nm "x"  1 6
    ; op "+"  1 8
    ; nm "y"  1 10
    ; rp      1 11
    ]
    [ TName "f"; TOp "←"; LParen; TName "x"; TOp "+"; TName "y"; RParen ]

(** ─────────────────────────────────────────────────────
    Single-line form: square ← x → x * x
    Both contexts opened and closed at EOF with VSemi. *)
let t_single_line_form () =
  check_eq "single_line_form"
    [ nm "square" 1 1
    ; op "←"      1 8
    ; nm "x"      1 10
    ; op "→"      1 12
    ; nm "x"      1 14
    ; op "*"      1 16
    ; nm "x"      1 18
    ]
    [ TName "square"; TOp "←"; TName "x"; TOp "→"
    ; TName "x"; TOp "*"; TName "x"
    ; VSemi; VSemi   (* EOF closes both ← and → contexts *)
    ]

(** ─────────────────────────────────────────────────────
    Multi-branch layout:
      isZero ←
        0 → True
        _ → False

    First branch is NOT preceded by VSemi (suppressed by first_line rule).
    The _ branch is preceded by a VSemi separating it from the True branch.
    Two VSemis at EOF close the → and ← contexts. *)
let t_multi_branch () =
  check_eq "multi_branch_layout"
    [ nm "isZero" 1 1
    ; op "←"      1 8   (* opens layout C=3, first_line=2 *)
    ; lit "0"     2 3
    ; op "→"      2 5   (* opens layout C=9, first_line=2 *)
    ; nm "True"   2 9
    ; nm "_"      3 3   (* N=3: pop C=9, then N=3=C=3 → VSemi *)
    ; op "→"      3 5   (* opens layout C=9, first_line=3 *)
    ; nm "False"  3 9
    ]
    [ TName "isZero"; TOp "←"
    ; TLit "0"; TOp "→"; TName "True"
    ; VSemi                              (* separates the two branches *)
    ; TName "_"; TOp "→"; TName "False"
    ; VSemi; VSemi                       (* EOF closes → and ← contexts *)
    ]

(** ─────────────────────────────────────────────────────
    Multi-statement block — sequential bindings:
      result ←
        x ← f a
        x + 1

    The 'x ← f a' line does not get a leading VSemi (first_line rule).
    'x + 1' on the next line at the same column gets a VSemi before it. *)
let t_sequential_bindings () =
  check_eq "sequential_bindings"
    [ nm "result" 1 1
    ; op "←"      1 8   (* opens C=3, first_line=2 *)
    ; nm "x"      2 3
    ; op "←"      2 5   (* opens C=7, first_line=2 *)
    ; nm "f"      2 7
    ; nm "a"      2 9
    ; nm "x"      3 3   (* N=3 < C=7 → pop; N=3=C=3 → VSemi *)
    ; op "+"      3 5
    ; lit "1"     3 7
    ]
    [ TName "result"; TOp "←"
    ; TName "x"; TOp "←"; TName "f"; TName "a"
    ; VSemi
    ; TName "x"; TOp "+"; TLit "1"
    ; VSemi          (* EOF closes only the outer ← context *)
    ]

(** ─────────────────────────────────────────────────────
    Next-line ( is NOT same-line suppression:
      g ←
        (x + y)

    The ( is on the NEXT line after ←, so a layout context is opened with
    C = col(().  Inside the parens bracket_depth = 1, so the three-case
    rule is suppressed.  After ) the context on the stack is still present
    and closes at EOF. *)
let t_next_line_paren_no_suppression () =
  check_eq "next_line_paren_opens_context"
    [ nm "g"  1 1
    ; op "←"  1 3   (* opens C=3, first_line=2 *)
    ; lp      2 3   (* N=3, line=2=first_line=2 → not active, no VSemi; bracket_depth→1 *)
    ; nm "x"  2 4
    ; op "+"  2 6
    ; nm "y"  2 8
    ; rp      2 9   (* bracket_depth→0 *)
    ]
    [ TName "g"; TOp "←"
    ; LParen; TName "x"; TOp "+"; TName "y"; RParen
    ; VSemi   (* EOF closes ← context *)
    ]

(** ─────────────────────────────────────────────────────
    Inside explicit ( ) newlines are insignificant.
    This function type spans two lines but is inside parens — no VSemi. *)
let t_inside_parens_no_layout () =
  check_eq "inside_parens_no_layout"
    [ lp       1 1
    ; nm "Int" 1 2
    ; op "→"   1 6   (* inside parens: → still a block-intro but bracket_depth=1 *)
    ; nm "Bool"2 3   (* new line, but bracket_depth=1 → no layout rule *)
    ; rp       2 8
    ]
    [ LParen; TName "Int"; TOp "→"; TName "Bool"; RParen ]
    (* No VSemi: the → inside parens sees bracket_depth=1 after the LParen
       increments it, so no context is pushed and no rule fires on the newline. *)

(** ─────────────────────────────────────────────────────
    Nested blocks:
      classify ←
        x when x > 0 →
          y ← transform x
          y * 2
        _ → 0

    Depth: ← opens at col 3 (first_line=2).
           First → opens at col 11 (first_line=2).
           Inner ← opens at col 15 (first_line=3).
           Second outer-level branch starts at col 3 line 5. *)
let t_nested_blocks () =
  check_eq "nested_blocks"
    [ nm "classify"  1 1
    ; op "←"         1 10  (* opens C=3, first_line=2 *)
    ; nm "x"         2 3
    ; nm "when"      2 5
    ; nm "x"         2 10
    ; op ">"         2 12
    ; lit "0"        2 14
    ; op "→"         2 16  (* opens C=11, first_line=3 *)
                           (* (next token y is at 3,11) *)
    ; nm "y"         3 11
    ; op "←"         3 13  (* opens C=21, first_line=3 *)
                           (* (next token 'transform' at 3,15) *)
    ; nm "transform" 3 15
    ; nm "x"         3 25
    ; nm "y"         4 11  (* N=11: pop C=21(inner←); N=11=C=11 → VSemi *)
    ; op "*"         4 13
    ; lit "2"        4 15
    ; nm "_"         5 3   (* N=3: pop C=11(→); pop → N=3=C=3(outer←) → VSemi *)
    ; op "→"         5 5   (* opens C=7, first_line=5 *)
    ; lit "0"        5 7
    ]
    [ TName "classify"; TOp "←"
    ; TName "x"; TName "when"; TName "x"; TOp ">"; TLit "0"; TOp "→"
    ; TName "y"; TOp "←"; TName "transform"; TName "x"
    ; VSemi                        (* separates y←transform from y*2 *)
    ; TName "y"; TOp "*"; TLit "2"
    ; VSemi                        (* separates first branch from _ → 0 *)
    ; TName "_"; TOp "→"; TLit "0"
    ; VSemi; VSemi                 (* EOF closes →(5,5) and outer ← contexts *)
    ]

(** ─────────────────────────────────────────────────────
    'handle block (SYNTAX.md §1.7 'handle block layout):
      comp 'handle
        ctl fail e   → default
        return x     → x Ok    *)
let t_handle_block () =
  check_eq "handle_block_layout"
    [ nm "comp"    1 1
    ; nm "'handle" 1 6   (* opens C=3, first_line=2 *)
    ; nm "ctl"     2 3
    ; nm "fail"    2 7
    ; nm "e"       2 12
    ; op "→"       2 14  (* opens C=24, first_line=2 *)
    ; nm "default" 2 16
    ; nm "return"  3 3   (* N=3: pop C=24; N=3=C=3 → VSemi *)
    ; nm "x"       3 10
    ; op "→"       3 12  (* opens C=16, first_line=3 *)
    ; nm "x"       3 14
    ; nm "Ok"      3 16
    ]
    [ TName "comp"; TName "'handle"
    ; TName "ctl"; TName "fail"; TName "e"; TOp "→"; TName "default"
    ; VSemi                         (* separates ctl clause from return clause *)
    ; TName "return"; TName "x"; TOp "→"; TName "x"; TName "Ok"
    ; VSemi; VSemi                  (* EOF closes → and 'handle contexts *)
    ]

(** ─────────────────────────────────────────────────────
    iso block (SYNTAX.md §1.7 iso block layout):
      iso A B
        from A → B ← forward_fn
        from B → A ← backward_fn   *)
let t_iso_block () =
  check_eq "iso_block_layout"
    [ nm "iso"         1 1
    ; nm "A"           1 5
    ; nm "B"           1 7   (* next token 'from' is at (2,3) — not same-line ( *)
                              (* iso opens C=3, first_line=2 *)
    ; nm "from"        2 3
    ; nm "A"           2 8
    ; op "→"           2 10  (* opens C=12, first_line=2 *)
    ; nm "B"           2 12
    ; op "←"           2 14  (* opens C=26, first_line=2 *)
    ; nm "forward_fn"  2 16
    ; nm "from"        3 3   (* N=3: pop C=26, pop C=12; N=3=C=3 → VSemi *)
    ; nm "B"           3 8
    ; op "→"           3 10  (* opens C=12, first_line=3 *)
    ; nm "A"           3 12
    ; op "←"           3 14  (* opens C=27, first_line=3 *)
    ; nm "backward_fn" 3 16
    ]
    [ TName "iso"; TName "A"; TName "B"
    ; TName "from"; TName "A"; TOp "→"; TName "B"; TOp "←"; TName "forward_fn"
    ; VSemi                          (* separates the two from_impl entries *)
    ; TName "from"; TName "B"; TOp "→"; TName "A"; TOp "←"; TName "backward_fn"
    ; VSemi; VSemi; VSemi            (* EOF closes ←(3,14), →(3,10), and iso contexts *)
    ]

(** ─────────────────────────────────────────────────────
    Continuation line — indented past reference column, no VSemi.
      result ←
        someVeryLong +
        expression           *)
let t_continuation_line () =
  check_eq "continuation_line_no_vsemi"
    [ nm "result"       1 1
    ; op "←"            1 8   (* opens C=3, first_line=2 *)
    ; nm "someVeryLong" 2 3
    ; op "+"            2 15
    ; nm "expression"   3 3   (* N=3=C=3 → VSemi; this is a new statement *)
    ]
    [ TName "result"; TOp "←"
    ; TName "someVeryLong"; TOp "+"
    ; VSemi                      (* N=3=C=3: new statement at same column *)
    ; TName "expression"
    ; VSemi                      (* EOF closes ← context *)
    ]

(** ─────────────────────────────────────────────────────
    ASCII aliases: <- and -> behave identically to ← and →. *)
let t_ascii_aliases () =
  check_eq "ascii_arrow_aliases"
    [ nm "f"  1 1
    ; op "<-" 1 3   (* opens C=5, first_line=1 — same line *)
    ; nm "x"  1 5
    ; op "->" 1 7   (* opens C=9, first_line=1 — same line *)
    ; nm "y"  1 9
    ]
    [ TName "f"; TOp "<-"; TName "x"; TOp "->"; TName "y"
    ; VSemi; VSemi
    ]

(** ─────────────────────────────────────────────────────
    EOF with no open contexts emits no VSemis. *)
let t_eof_no_contexts () =
  check_eq "eof_no_contexts"
    [ nm "x" 1 1; op "+" 1 3; nm "y" 1 5 ]
    [ TName "x"; TOp "+"; TName "y" ]

(* ── runner ─────────────────────────────────────────────────────────────── *)

let all_tests =
  [ "empty",                    t_empty
  ; "single",                   t_single
  ; "explicit_semi",            t_explicit_semi
  ; "same_line_paren",          t_same_line_paren_suppression
  ; "single_line_form",         t_single_line_form
  ; "multi_branch",             t_multi_branch
  ; "sequential_bindings",      t_sequential_bindings
  ; "next_line_paren",          t_next_line_paren_no_suppression
  ; "inside_parens_no_layout",  t_inside_parens_no_layout
  ; "nested_blocks",            t_nested_blocks
  ; "handle_block",             t_handle_block
  ; "iso_block",                t_iso_block
  ; "continuation_line",        t_continuation_line
  ; "ascii_aliases",            t_ascii_aliases
  ; "eof_no_contexts",          t_eof_no_contexts
  ]

let () =
  let pass = ref 0 and fail = ref 0 in
  List.iter (fun (name, f) ->
    if f () then incr pass
    else (incr fail; Printf.printf "  ^ in test: %s\n" name)
  ) all_tests;
  Printf.printf "%d passed, %d failed\n" !pass !fail;
  if !fail > 0 then exit 1
