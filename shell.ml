open Ast.AST
open Ast.Type
open Typed_ast.Typed_AST

let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let print a = Ast.AST.string_of_program a

let prompt () = print_string "> "; flush stdout

let rec shell () =
  prompt ();
  print_endline @@ print @@ parse @@ input_line stdin;
  shell ()

let rec lowershell () =
  let open Ast0.AST0 in
  prompt ();
  let (e, l) = lower_prog @@ parse @@ input_line stdin in
  print_endline @@ string_of_program @@ l;
  lowershell ()

let rec umshell () =
  prompt ();
  let ast = parse @@ input_line stdin in
  let (e, l) = Ast0.AST0.lower_prog @@ ast in
  let p = Ast1.AST1.lower_prog @@ l in
  print_endline @@ Ast1.AST1.string_of_prog p;
  umshell ()

let () =
  if Array.length Sys.argv > 1 then
    if Sys.argv.(1) = "lower" then
      lowershell ()
    else if Sys.argv.(1) = "ast" then
      shell ()
    else if Sys.argv.(1) = "um" then
      umshell ()
    else
      failwith "unknown shell"
  else
    failwith "no shell given"
