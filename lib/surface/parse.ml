(** Arra parser glue — lexer → layout → Menhir.

    The pipeline is:
      1. [Lexer.lex] converts raw source text to a [Layout.token_loc list].
      2. [Layout.run] inserts virtual-semicolon tokens.
      3. [adapt] converts each [Layout.token_loc] to a [Parser] token plus
         Menhir position pair.
      4. A token-supplier closure drives the Menhir incremental engine.

    Error reporting
    ───────────────
    Lex errors raise [Lexer.LexError].  Parse errors raise [Error] with a
    human-readable message including the file name and position. *)

exception Error of string

(* ── Keyword map ──────────────────────────────────────────────────────── *)

(** Convert a raw lowercase / mixed-case name string to the appropriate
    Menhir keyword token, or return [None] if it is a plain identifier. *)
let keyword_of_lower = function
  | "module"         -> Some Parser.MODULE
  | "interface"      -> Some Parser.INTERFACE
  | "implementation" -> Some Parser.IMPLEMENTATION
  | "trait"          -> Some Parser.TRAIT
  | "rec"            -> Some Parser.REC
  | "effect"         -> Some Parser.EFFECT
  | "type"           -> Some Parser.TYPE_KW
  | "alias"          -> Some Parser.ALIAS_KW
  | "distinct"       -> Some Parser.DISTINCT
  | "derived"        -> Some Parser.DERIVED
  | "open"           -> Some Parser.OPEN
  | "when"           -> Some Parser.WHEN
  | "extern"         -> Some Parser.EXTERN
  | "iso"            -> Some Parser.ISO
  | "from"           -> Some Parser.FROM
  | "forall"         -> Some Parser.FORALL
  | "exists"         -> Some Parser.EXISTS
  | "ctl"            -> Some Parser.CTL
  | "op"             -> Some Parser.OP_KW
  | "return"         -> Some Parser.RETURN
  | "bop"            -> Some Parser.BOP
  | "uop"            -> Some Parser.UOP
  | "app"            -> Some Parser.APP
  | "pat"            -> Some Parser.PAT
  | "project"        -> Some Parser.PROJECT
  | "assign"         -> Some Parser.ASSIGN
  | "view"           -> Some Parser.VIEW
  | "iter"           -> Some Parser.ITER
  | "gen"            -> Some Parser.GEN
  | "Never"          -> Some Parser.NEVER   (* uppercase alias for ⊥ *)
  | _                -> None

(** Convert an operator string to the appropriate structural token (if it is
    one of the reserved operators), or return [Parser.OP s] otherwise. *)
let tok_of_op s =
  match s with
  | "→" | "->" -> Parser.ARROW
  | "⇒" | "=>" -> Parser.DARROW
  | "←" | "<-" -> Parser.LARROW
  | "."        -> Parser.DOT
  | ".."       -> Parser.DOTDOT
  | "@"        -> Parser.AT
  | ","        -> Parser.COMMA
  | ":"        -> Parser.COLON
  | "⟪"        -> Parser.LEFF
  | "⟫"        -> Parser.REFF
  | _          -> Parser.OP s

(** Classify a raw [TLit] string to detect the correct token. *)
let token_of_lit s =
  let n = String.length s in
  if n = 0 then Parser.INT "0"
  else match s.[0] with
  | '"'  -> Parser.STRING (String.sub s 1 (n - 2))
  | '`'  -> Parser.SYMBOL (String.sub s 1 (n - 1))
  | _    ->
    if String.contains s '.' ||
       ((String.contains s 'e' || String.contains s 'E')
        && not (String.contains s 'x' || String.contains s 'X'))
    then Parser.FLOAT (float_of_string
           (String.concat "" (String.split_on_char '_' s)))
    else Parser.INT s

(* ── Adapter ──────────────────────────────────────────────────────────── *)

(** Menhir expects positions as [Lexing.position] pairs.  We build minimal
    synthetic positions from [Layout.pos]. *)
let menhir_pos filename (p : Layout.pos) : Lexing.position =
  { pos_fname = filename
  ; pos_lnum  = p.line
  ; pos_bol   = 0
  ; pos_cnum  = p.col - 1   (* 0-indexed byte offset approximation *)
  }

let fake_mpos = Lexing.dummy_pos

(** Convert a single [Layout.token_loc] to a [(Parser.token, Lexing.position,
    Lexing.position)] triple consumed by the Menhir incremental API. *)
let adapt filename ({ token; pos } : Layout.token_loc)
    : Parser.token * Lexing.position * Lexing.position =
  let mp = menhir_pos filename pos in
  let tok = match token with
    | Layout.TName s ->
      (match s.[0] with
       | 'A'..'Z' ->
         (match keyword_of_lower s with
          | Some t -> t
          | None   -> Parser.UPPER s)
       | '\'' ->
         (* Tick name: TName "'foo" → TICK "foo" *)
         Parser.TICK (String.sub s 1 (String.length s - 1))
       | _ ->
         (match keyword_of_lower s with
          | Some t -> t
          | None   -> Parser.LOWER s))
    | Layout.TOp s   -> tok_of_op s
    | Layout.TLit s  -> token_of_lit s
    | Layout.LParen       -> Parser.LPAREN
    | Layout.RParen       -> Parser.RPAREN
    | Layout.LBracket     -> Parser.LBRACKET
    | Layout.RBracket     -> Parser.RBRACKET
    | Layout.LBrace       -> Parser.LBRACE
    | Layout.RBrace       -> Parser.RBRACE
    | Layout.LEffect      -> Parser.LEFF
    | Layout.REffect      -> Parser.REFF
    | Layout.BacktickLBracket -> Parser.BACKTICK_LBRACKET
    | Layout.AttrOpen     -> Parser.ATTR_OPEN
    | Layout.AttrClose    -> Parser.ATTR_CLOSE
    | Layout.Semi         -> Parser.SEMI
    | Layout.VSemi        -> Parser.VSEMI
    | Layout.EOF          -> Parser.EOF
  in
  (tok, mp, mp)

(* ── Public API ──────────────────────────────────────────────────────── *)

(** Parse an Arra source file.
    @param filename  Used in error messages and position info.
    @param source    Full source text. *)
let parse_string ~filename source : Ast.program =
  (* Step 1: lex *)
  let raw_tokens =
    try Lexer.lex ~filename source
    with Lexer.LexError (msg, pos) ->
      raise (Error (Printf.sprintf "%s:%d:%d: lex error: %s"
                      filename pos.Layout.line pos.Layout.col msg))
  in
  (* Step 2: layout preprocessing (insert VSemi) *)
  let tokens = Layout.run raw_tokens in
  (* Step 3: build a token supplier *)
  let q = Queue.of_seq (List.to_seq tokens) in
  let supplier () =
    if Queue.is_empty q then (Parser.EOF, fake_mpos, fake_mpos)
    else adapt filename (Queue.pop q)
  in
  (* Step 4: run the Menhir parser *)
  try MenhirLib.Convert.Simplified.traditional2revised
        Parser.program
        supplier
  with Parser.Error ->
    raise (Error (Printf.sprintf "%s: parse error" filename))
