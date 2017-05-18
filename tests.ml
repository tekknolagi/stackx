open Ast.AST
open Ast.Type

let parse s = Parser.main Lexer.token @@ Lexing.from_string s
let a str tree = let _ = assert ((parse str)=tree) in ()

let exp_s s = "func a() : int { " ^ s ^ "}"
let exp_v e = Prog [(Fun ("a", [], Int, [e]))]

let () =
  (
  a (exp_s "1 + 2 - 3;")
    (exp_v (Exp (MathOper (Minus, MathOper (Plus, IntLit 1, IntLit 2), IntLit 3))));
  (*
  let _ = assert ((parse "func a() : int { let a : int = 4; }")=(Assignment(LLet, ("a", Int), (IntLit 4)))) in
  let _ = assert ((parse "func a() : int { if (3) { 4; } }")=(If (IntLit 3, [Exp (IntLit 4)]))) in
  let _ = assert ((parse "func a() : int { if (3) { 4; } else { 5; } }")=(IfElse (IntLit 3, [Exp (IntLit 4)], [Exp (IntLit 5)]))) in
  let _ = assert ((parse "func a() : int { hello(); }")=(Exp (Funcall ("hello", [])))) in
  let _ = assert ((parse "func a() : int { hello(1); }")=(Exp (Funcall ("hello", [IntLit 1])))) in
  let _ = assert ((parse "func a() : int { hello(1, 2, 3); }")=(Exp (Funcall ("hello", [IntLit 1; IntLit 2; IntLit 3])))) in
  let _ = assert ((parse "func a() : int { hello(a(1)); }")=(Exp (Funcall ("hello", [Funcall ("a", [IntLit 1])])))) in
  let _ = assert ((parse "func a() : int { return 10; }")=(Return (IntLit 10))) in
  let _ = assert ((parse "func a() : int { return a(1); }")=(Return (Funcall ("a", [IntLit 1])))) in
  let _ = assert ((parse "func a() : int { 1 < 3; }")=(Exp (CompOper (Lt, IntLit 1, IntLit 3)))) in
*)
  )
