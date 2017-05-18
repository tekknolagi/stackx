let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let () =
  let open Ast.AST in
  let open Ast.Type in
  let _ = assert ((parse "1 + 2 - 3;")=(Exp (MathOper (Minus, MathOper (Plus, IntLit 1, IntLit 2), IntLit 3)))) in
  let _ = assert ((parse "let a : int = 4;")=(Assignment(LLet, ("a", Int), (IntLit 4)))) in
  let _ = assert ((parse "if (3) { 4; }")=(If (IntLit 3, [Exp (IntLit 4)]))) in
  let _ = assert ((parse "if (3) { 4; } else { 5; }")=(IfElse (IntLit 3, [Exp (IntLit 4)], [Exp (IntLit 5)]))) in
  let _ = assert ((parse "hello();")=(Exp (Funcall ("hello", [])))) in
  let _ = assert ((parse "hello(1);")=(Exp (Funcall ("hello", [IntLit 1])))) in
  let _ = assert ((parse "hello(1, 2, 3);")=(Exp (Funcall ("hello", [IntLit 1; IntLit 2; IntLit 3])))) in
  let _ = assert ((parse "return 10;")=(Return (IntLit 10))) in
  let _ = assert ((parse "return a(1);")=(Return (Funcall ("a", [IntLit 1])))) in
  ()
