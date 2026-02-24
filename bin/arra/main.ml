let () =
  let args = Sys.argv in
  if Array.length args < 2 then (
    Printf.eprintf "arra: no input file\n";
    Printf.eprintf "usage: arra <file.ar>\n";
    exit 1
  );
  let path = args.(1) in
  match Driver.Pipeline.run_treewalk (Driver.Pipeline.File path) with
  | Driver.Pipeline.Interpreted _  -> ()
  | Driver.Pipeline.Compiled    _  -> ()
  | Driver.Pipeline.ParseError  e  ->
    Printf.eprintf "parse error: %s\n" e; exit 1
  | Driver.Pipeline.TypeError   e  ->
    Printf.eprintf "type error: %s\n" e; exit 1
