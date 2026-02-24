(** Pipeline driver — coordinates the compiler stages.
    See doc/FRONTEND.md for the full pipeline specification. *)

(** Pipeline stages (stubs — filled in as each stage is implemented):
    1. Lex + Parse       → Surface.Ast.program
    2. Type check        → typed AST        (TODO)
    3. Hook resolution   → resolved AST     (TODO)
    4. Core lowering     → Core_ir.program  (TODO)
    5. Whole-program passes (mono, fusion)  (TODO)
    6a. Treewalk         → value            (interpreter)
    6b. C emission       → string           (compiler, TODO)  *)

type input = File of string | Str of string

type pipeline_result =
  | Interpreted of Treewalk.Eval.value
  | Compiled    of string   (** emitted C source *)
  | ParseError  of string
  | TypeError   of string

(** Stub: parse an input source *)
let parse (_input : input) : (Surface.Ast.program, string) result =
  Error "parser not yet implemented"

(** Stub: run the full treewalk pipeline on source *)
let run_treewalk (input : input) : pipeline_result =
  match parse input with
  | Error msg -> ParseError msg
  | Ok _ast   -> ParseError "frontend not yet implemented"
  |> ignore;
  ignore input;
  ParseError "pipeline not yet implemented"
