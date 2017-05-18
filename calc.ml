(* File calc.ml *)
(*
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      print_int result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0
    *)

let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let () =
  let open Ast.AST in
  let _ = assert ((parse "1 + 2 - 3\n")=(InfixOper (Minus, InfixOper (Plus, IntLit 1, IntLit 2), IntLit 3))) in
  ()

(*
let _ =
  let buf = Lexing.from_string "1 + 2 - 3\n" in
  let result = Parser.main Lexer.token buf in
  print_endline @@ Ast.AST.string_of_exp result
  *)
