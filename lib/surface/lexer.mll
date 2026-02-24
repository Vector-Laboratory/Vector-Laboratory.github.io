(** Arra lexer — converts a source file into a [Layout.token_loc list].

    Handles Unicode/ASCII duality for reserved tokens (←→⇒⟪⟫⊥∀∃) and
    their ASCII aliases (<- -> => <\ \> Never forall exists).  All other
    non-whitespace, non-identifier code points are op_chars (greedy munch).

    Column numbers are byte-indexed + 1 (1-indexed).  For ASCII-only source
    this is identical to code-point column counting; correcting for multi-byte
    Unicode identifiers is a future TODO.

    Tab characters at indentation-sensitive positions are a lexer error per
    the spec; inside string literals and comments they are allowed. *)

{
open Layout

exception LexError of string * pos

(** Build a position from the START of the current lexeme. *)
let make_pos lb =
  let p = Lexing.lexeme_start_p lb in
  { line = p.pos_lnum; col = p.pos_cnum - p.pos_bol + 1 }

let mk tok lb = { token = tok; pos = make_pos lb }

(** Unescape the content of a string literal (quotes already stripped). *)
let unescape s =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 and n = String.length s in
  while !i < n do
    if s.[!i] = '\\' && !i + 1 < n then begin
      (match s.[!i + 1] with
      | 'n'  -> Buffer.add_char buf '\n'
      | 't'  -> Buffer.add_char buf '\t'
      | 'r'  -> Buffer.add_char buf '\r'
      | '"'  -> Buffer.add_char buf '"'
      | '\\' -> Buffer.add_char buf '\\'
      | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c);
      i := !i + 2
    end else begin
      Buffer.add_char buf s.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf
}

(* ── Character classes ──────────────────────────────────────────────────── *)

let lower  = ['a'-'z']
let upper  = ['A'-'Z']
let digit  = ['0'-'9']
let hex    = ['0'-'9' 'a'-'f' 'A'-'F']
let ident  = ['a'-'z' 'A'-'Z' '0'-'9' '_']

(* ASCII operator characters.
   Excluded: . (handled separately as DOT/DOTDOT), : , ; ( ) [ ] { } ` @ '
   Note: @ is an op_char per spec but used only in as-patterns — kept as op.
   Note: ' is an op_char but starts tick names when followed by [a-z]. *)
let aop    = ['+' '-' '*' '/' '%' '^' '&' '|' '!' '~' '=' '<' '>' '?'
               '@' '#' '$' '\\' '\'']

(* High bytes of UTF-8 multi-byte sequences treated as op_chars.
   This covers all non-ASCII Unicode that isn't one of the specifically
   recognised reserved sequences matched earlier in the rules. *)
let hi     = ['\x80'-'\xFF']

let op_char = aop | hi

(* ── Reserved UTF-8 sequences ───────────────────────────────────────────── *)
(* Each is the exact UTF-8 encoding of the code point. *)

let u_arrow  = "\xe2\x86\x92"   (* → U+2192 *)
let u_darrow = "\xe2\x87\x92"   (* ⇒ U+21D2 *)
let u_larrow = "\xe2\x86\x90"   (* ← U+2190 *)
let u_forall = "\xe2\x88\x80"   (* ∀ U+2200 *)
let u_exists = "\xe2\x88\x83"   (* ∃ U+2203 *)
let u_leff   = "\xe2\x9f\xaa"   (* ⟪ U+27EA *)
let u_reff   = "\xe2\x9f\xab"   (* ⟫ U+27EB *)
let u_never  = "\xe2\x8a\xa5"   (* ⊥ U+22A5 *)

(* ── Main lexer rule ─────────────────────────────────────────────────────── *)

rule next lb = parse

  (* ── Whitespace ── *)
  | ' '               { next lb lexbuf }
  | '\t'              { next lb lexbuf }   (* tabs outside indentation are ok *)
  | '\r' '\n'         { Lexing.new_line lexbuf; next lb lexbuf }
  | '\r'              { Lexing.new_line lexbuf; next lb lexbuf }
  | '\n'              { Lexing.new_line lexbuf; next lb lexbuf }

  (* ── Comments ── *)
  | "//" [^ '\n']*    { next lb lexbuf }   (* line comment *)
  | "/'"              { block_comment 0 lexbuf; next lb lexbuf }

  (* ── Attribute markers  /'-  -'/
     Must come before the general operator rule since /'-  starts with /. *)
  | "/'-"             { mk AttrOpen  lb }
  | "-'/"             { mk AttrClose lb }

  (* ── Brackets ── *)
  | '('               { mk LParen   lb }
  | ')'               { mk RParen   lb }
  | '['               { mk LBracket lb }
  | ']'               { mk RBracket lb }
  | '{'               { mk LBrace   lb }
  | '}'               { mk RBrace   lb }
  | u_leff            { mk LEffect  lb }
  | u_reff            { mk REffect  lb }

  (* ASCII effect-row brackets  <\  \>  (Tier-3 watcher substitution aliases)
     Per spec §2: substitute only when opening is followed by non-op_char.
     In the lexer we just emit them directly since they're just aliases. *)
  | "<\\" [^ '+' '-' '*' '/' '%' '^' '&' '|' '!' '~' '=' '<' '>'
              '?' '@' '#' '$' '\\' '\'' '\x80'-'\xFF']
    { (* rewind one char — the non-op_char is NOT part of this token *)
      let pos = Lexing.lexeme_end lexbuf in
      lexbuf.Lexing.lex_curr_pos <- pos - 1;
      mk LEffect lb }
  | "\\>"             { mk REffect lb }

  (* ── Semicolon and comma/colon (reserved punctuation, not operators) ── *)
  | ';'               { mk Semi lb }
  | ','               { mk (TOp ",") lb }
  | ':'               { mk (TOp ":") lb }

  (* ── Backtick: `[ (quoted-program opener) or `name (symbol) ── *)
  | '`' '['           { mk BacktickLBracket lb }
  | '`' (lower ident* as s)  { mk (TLit ("`" ^ s)) lb }
  | '`' (upper ident* as s)  { mk (TLit ("`" ^ s)) lb }

  (* ── Reserved arrows and quantifiers — must precede op_char+ rule ── *)
  | u_arrow    { mk (TOp "→") lb }
  | u_darrow   { mk (TOp "⇒") lb }
  | u_larrow   { mk (TOp "←") lb }
  | u_forall   { mk (TName "forall") lb }
  | u_exists   { mk (TName "exists") lb }
  | u_never    { mk (TName "Never")  lb }

  (* ASCII aliases for arrows (must precede general op rule) *)
  | "->"       { mk (TOp "→") lb }
  | "=>"       { mk (TOp "⇒") lb }
  | "<-"       { mk (TOp "←") lb }

  (* ── Dot(s): .. (DOTDOT) or . (DOT) — must precede op_char+ ── *)
  | ".."       { mk (TOp "..") lb }
  | "."        { mk (TOp ".")  lb }

  (* ── String literals ── *)
  | '"' '"' '"'
    { let p   = make_pos lb in
      let buf = Buffer.create 64 in
      triple_string buf lexbuf;
      { token = TLit ("\"" ^ Buffer.contents buf ^ "\""); pos = p } }
  | '"'
    { let p   = make_pos lb in
      let buf = Buffer.create 32 in
      single_string buf lexbuf;
      { token = TLit ("\"" ^ unescape (Buffer.contents buf) ^ "\""); pos = p } }

  (* ── Numeric literals ── *)
  (* Float: must come before integer to match the decimal point greedily *)
  | (digit (digit | '_')*) '.'
    (digit (digit | '_')*)
    (('e' | 'E') ('+' | '-')? digit+)?  as s
    { mk (TLit s) lb }

  | "0x" (hex | '_')+ as s   { mk (TLit s) lb }
  | "0b" (['0' '1'] | '_')+ as s   { mk (TLit s) lb }
  | "0o" (['0'-'7'] | '_')+ as s   { mk (TLit s) lb }
  | digit (digit | '_')* as s       { mk (TLit s) lb }

  (* ── Tick names: 'lower — must precede op_char+ ── *)
  | '\'' (lower ident* as s)   { mk (TName ("'" ^ s)) lb }

  (* ── Identifiers ── *)
  | upper ident* as s   { mk (TName s) lb }
  | lower ident* as s   { mk (TName s) lb }

  (* ── Operators: maximal munch of op_chars ── *)
  | op_char+ as s   { mk (TOp s) lb }

  (* ── EOF ── *)
  | eof   { mk EOF lb }

  (* ── Error ── *)
  | _ as c
    { raise (LexError
        (Printf.sprintf "unexpected character %C" c, make_pos lb)) }

(* ── Block comment (nestable) ─────────────────────────────────────────── *)

and block_comment depth = parse
  | "/'"    { block_comment (depth + 1) lexbuf }
  | "'/"    { if depth > 0 then block_comment (depth - 1) lexbuf }
  | '\n'    { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | '\r' '\n' { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | '\r'    { Lexing.new_line lexbuf; block_comment depth lexbuf }
  | eof     { failwith "unterminated block comment" }
  | _       { block_comment depth lexbuf }

(* ── Single-line string body (between the opening and closing quote) ──────── *)

and single_string buf = parse
  | '"'       { () }
  | '\\' '"'  { Buffer.add_char buf '\\'; Buffer.add_char buf '"';
                single_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\\'; Buffer.add_char buf 'n';
                single_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\\'; Buffer.add_char buf 't';
                single_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\\'; Buffer.add_char buf 'r';
                single_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; Buffer.add_char buf '\\';
                single_string buf lexbuf }
  | '\\' (_ as c)
    { Buffer.add_char buf '\\'; Buffer.add_char buf c;
      single_string buf lexbuf }
  | '\n'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n';
      single_string buf lexbuf }
  | eof       { failwith "unterminated string literal" }
  | _ as c    { Buffer.add_char buf c; single_string buf lexbuf }

(* ── Triple-quoted string body (raw — no escape processing) ───────────── *)

and triple_string buf = parse
  | '"' '"' '"'   { () }
  | '\n'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n';
      triple_string buf lexbuf }
  | '\r' '\n'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n';
      triple_string buf lexbuf }
  | '\r'
    { Lexing.new_line lexbuf; Buffer.add_char buf '\n';
      triple_string buf lexbuf }
  | eof         { failwith "unterminated triple-quoted string" }
  | _ as c      { Buffer.add_char buf c; triple_string buf lexbuf }

(* ── Public entry point ──────────────────────────────────────────────── *)

{
(** Lex an entire source file into a token list.
    @param filename  used for error messages (stored in lexbuf position)
    @param source    full source text *)
let lex ~filename source : token_loc list =
  let lb = Lexing.from_string source in
  Lexing.set_filename lb filename;
  let rec loop acc =
    let tok = next lb lb in
    match tok.token with
    | EOF -> List.rev (tok :: acc)
    | _   -> loop (tok :: acc)
  in
  loop []
}
