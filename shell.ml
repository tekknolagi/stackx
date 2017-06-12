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

let () =
  shell ()
