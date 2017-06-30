open Ast.AST
open Ast.Type
open Typed_ast.Typed_AST

let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let print a = Ast.AST.string_of_program a

let read s = print @@ parse s

let rec shell () =
  print_string "> ";
  flush stdout;
  print_endline @@ read @@ input_line stdin;
  shell ()

let rec lowershell () =
  let open Ast0.AST0 in
  print_string "> ";
  flush stdout;
  let (e, l) = lower_prog @@ parse @@ input_line stdin in
  print_endline @@ string_of_program @@ l;
  lowershell ()

let () =
  if Array.length Sys.argv > 1 then
    if Sys.argv.(1) = "lower" then
      lowershell ()
    else if Sys.argv.(1) = "ast" then
      shell ()
    else
      failwith "unknown shell"
  else
    failwith "no shell given"
