open Ast.AST
open Ast.Type

let parse s = Parser.main Lexer.token @@ Lexing.from_string s
let a str tree = let _ = assert ((parse str)=tree) in ()

let func_s s = "func a() : int { " ^ s ^ "}"
let stat_v s = Prog [Fun ("a", [], Int, [s])]
let exp_v e = stat_v (Exp e)

let () =
  (
  a (func_s "1 + 2 - 3;") (exp_v (InfixOper (Minus, InfixOper (Plus, IntLit 1, IntLit 2), IntLit 3)));
  a (func_s "let a : int = 4;") (stat_v (Let (LLet, ("a", Int), (IntLit 4))));
  a (func_s "const a : int = 4;") (stat_v (Let (LConst, ("a", Int), (IntLit 4))));
  a (func_s "if (3) { 4; }") (stat_v (If (IntLit 3, [Exp (IntLit 4)])));
  a (func_s "if (3) { 4; } else { 5; }") (stat_v (IfElse (IntLit 3, [Exp (IntLit 4)], [Exp (IntLit 5)])));
  a (func_s "hello();") (exp_v (Funcall ("hello", [])));
  a (func_s "hello(1);") (exp_v (Funcall ("hello", [IntLit 1])));
  a (func_s "hello(1, 2, 3);") (exp_v (Funcall ("hello", [IntLit 1; IntLit 2; IntLit 3])));
  a (func_s "hello(a(1));") (exp_v (Funcall ("hello", [Funcall ("a", [IntLit 1])])));
  a (func_s "return 10;") (stat_v (Return (IntLit 10)));
  a (func_s "return a(1);") (stat_v (Return (Funcall ("a", [IntLit 1]))));
  a (func_s "1 < 3;") (exp_v (InfixOper (Lt, IntLit 1, IntLit 3)));
  a (func_s "a + b;") (exp_v (InfixOper (Plus, Var "a", Var "b")));
  a (func_s "a := 2 + 3;") (stat_v (SetEq("a", (InfixOper (Plus, IntLit 2, IntLit 3)))));
  a (func_s "a && 1 + 2 >3;")
    (exp_v (InfixOper (And, Var "a", (InfixOper (Gt, (InfixOper (Plus, IntLit 1, IntLit 2)), IntLit 3)))));
  a "const a : int = 5; func b () : bool { 1; }"
    (Prog [Const (("a", Int), (IntLit 5)); Fun ("b", [], Bool, [Exp (IntLit 1)])]);
  a "const a : int = 5; func b () : bool { }"
    (Prog [Const (("a", Int), (IntLit 5)); Fun ("b", [], Bool, [])])
  )

let bigP s =
  Parser.main Lexer.token @@ Lexing.from_string s
