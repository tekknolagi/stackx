let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let () =
  let open Ast.AST in
  let open Ast.Type in
  let _ = assert ((parse "1 + 2 - 3;\n")=(Exp (InfixOper (Minus, InfixOper (Plus, IntLit 1, IntLit 2), IntLit 3)))) in
  let _ = assert ((parse "let a : int = 4;\n")=(Assignment(LLet, ("a", Int), (IntLit 4)))) in
  ()
