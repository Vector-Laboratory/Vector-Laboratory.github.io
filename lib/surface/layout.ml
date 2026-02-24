(** Layout preprocessor — inserts virtual semicolons into the token stream.

    Sits between the raw lexer and the Menhir grammar. The parser never
    inspects indentation; it sees only real tokens and virtual [VSemi] tokens.

    See doc/SYNTAX.md §1.7 for the full specification.

    Algorithm summary:
    1. Maintain a stack of layout contexts, each holding a reference column
       [col] and the line of the first token in that block [first_line].
    2. When a newline is crossed (outside explicit brackets), apply the
       three-case rule against the innermost context.
    3. Block-introducing tokens open a new layout context unless immediately
       followed by [(] on the same line (same-line suppression).
    4. Explicit [( )], [[ ]], [{ }] suspend the three-case rule while open.
    5. At EOF, close all remaining contexts (each emits a [VSemi]). *)

(* ── Token type ─────────────────────────────────────────────────────────── *)

(** Tokens produced by the lexer and consumed by the Menhir grammar.
    [VSemi] is never produced by the lexer — only injected here. *)
type token =
  | TName   of string  (** identifier, keyword, or tick-keyword ('handle …) *)
  | TOp     of string  (** operator: ←  →  <-  ->  +  -  …                 *)
  | TLit    of string  (** opaque literal: integer, float, string, char      *)
  | LParen             (** (  *)
  | RParen             (** )  *)
  | LBracket           (** [  *)
  | RBracket           (** ]  *)
  | LBrace             (** {  *)
  | RBrace             (** }  *)
  | LEffect            (** ⟪  — effect row open; suspends layout like (   *)
  | REffect            (** ⟫  — effect row close                           *)
  | BacktickLBracket   (** `[ — quoted program opener                       *)
  | AttrOpen           (** /'- — attribute block opener                     *)
  | AttrClose          (** -'/ — attribute block closer                     *)
  | Semi               (** explicit ;                                         *)
  | VSemi              (** virtual ; — injected by this module only           *)
  | EOF

type pos = { line : int; col  : int }

type token_loc = { token : token; pos : pos }

let fake_pos = { line = 0; col = 0 }

(* ── Block-introducing tokens ───────────────────────────────────────────── *)

(** Tokens that open a layout context when not immediately followed by
    a same-line [(].  See SYNTAX.md §1.7 block-introducing token table.

    Note: [→] / [->] here are the branch/lambda separators.  [→] as a
    function-type arrow appears inside explicit [( )] (layout suppressed)
    or in ': T → U' annotations; the extra context opened in type-annotation
    position is harmless — it closes on the next line without emitting [;]. *)
let is_block_intro = function
  | TName "module"         | TName "interface"
  | TName "implementation" | TName "trait"
  | TName "rec"            | TName "effect"
  | TName "iso"            | TName "'handle"
  | TOp "←" | TOp "<-"
  | TOp "→" | TOp "->"    -> true
  | _                      -> false

(* ── Layout context ─────────────────────────────────────────────────────── *)

(** One frame on the layout context stack.

    [col]        — reference column: column at which statements in this
                   block begin.
    [first_line] — line of the very first token in this block.  The
                   three-case rule is NOT fired for a token whose line
                   equals [first_line], preventing a spurious [VSemi] before
                   the first statement. *)
type ctx = { col : int; first_line : int }

(* ── Three-case rule ────────────────────────────────────────────────────── *)

(** [apply_newline ~n ~line ctx_stack] applies the three-case rule for the
    first token on a new line.  [n] is the column of that token, [line] its
    line number.

    Returns [(ctx_stack', vsemi_opt)] where [ctx_stack'] is the (possibly
    shorter) context stack after any pops, and [vsemi_opt] is [Some vsemi]
    if a virtual [;] should be prepended to the current token.

    Three cases (SYNTAX.md §1.7):
      n < C  → pop context, recurse (no [;] for this pop)
      n = C  → emit [VSemi], stop
      n > C  → no action, stop *)
let rec apply_newline ~n ~line = function
  | [] -> ([], None)
  | ctx :: rest ->
    if line <= ctx.first_line then
      (* Context just opened; first token not yet processed on a fresh line. *)
      (ctx :: rest, None)
    else if n < ctx.col then
      apply_newline ~n ~line rest          (* pop and continue *)
    else if n = ctx.col then
      (ctx :: rest, Some { token = VSemi; pos = { line; col = ctx.col } })
    else
      (ctx :: rest, None)                  (* continuation line — no action *)

(* ── Main pass ──────────────────────────────────────────────────────────── *)

(** [run tokens] inserts [VSemi] tokens into [tokens] according to the
    layout rule.

    Input:  positioned tokens from the lexer (whitespace already stripped;
            no explicit newline tokens — position info carries line numbers).
    Output: same tokens with [VSemi] entries injected. *)
let run (tokens : token_loc list) : token_loc list =
  let rec go ctx_stack bracket_depth prev_line acc = function

    | [] ->
      (* EOF: close every remaining layout context. *)
      let closing =
        List.map (fun _ -> { token = VSemi; pos = fake_pos }) ctx_stack
      in
      List.rev (List.rev_append closing acc)

    | ({ token; pos } as tok) :: rest ->

      (* Step 1 — three-case rule on line boundary, outside brackets. *)
      let ctx_stack, vsemi_opt =
        if pos.line > prev_line && bracket_depth = 0 then
          apply_newline ~n:pos.col ~line:pos.line ctx_stack
        else
          ctx_stack, None
      in
      let acc = match vsemi_opt with
        | None   -> acc
        | Some v -> v :: acc
      in

      (* Step 2 — track explicit bracket depth. *)
      let bracket_depth = match token with
        | LParen | LBracket | LBrace | LEffect -> bracket_depth + 1
        | RParen | RBracket | RBrace | REffect  -> bracket_depth - 1
        | _                                     -> bracket_depth
      in

      (* Step 3 — block-introducing token: push layout context if applicable.
                  Suppressed when followed by same-line [(].
                  Special case: [iso] uses the first [from] keyword as the
                  reference token (not the immediately next token), because
                  [iso A B] has header arguments before the block body.
                  See SYNTAX.md §1.7 iso block layout. *)
      let ctx_stack =
        if is_block_intro token && bracket_depth = 0 then
          let ref_pos_opt = match token with
            | TName "iso" ->
              (* Scan ahead to the first [from] keyword; that is the
                 reference token for the iso block, not [A] or [B]. *)
              (match List.find_opt (fun tl -> tl.token = TName "from") rest with
               | Some tl -> Some tl.pos
               | None    -> None)
            | _ ->
              (match rest with
               | [] -> None
               | next :: _ ->
                 if next.token = LParen && next.pos.line = pos.line then
                   None   (* same-line [(]: suppress *)
                 else
                   Some next.pos)
          in
          (match ref_pos_opt with
           | None      -> ctx_stack
           | Some rpos ->
             { col = rpos.col; first_line = rpos.line } :: ctx_stack)
        else
          ctx_stack
      in

      (* Step 4 — emit current token and advance. *)
      go ctx_stack bracket_depth pos.line (tok :: acc) rest

  in
  go [] 0 0 [] tokens
