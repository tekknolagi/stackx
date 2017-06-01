open Ast.AST
open Ast.Type
open Typed_ast.Typed_AST

let parse s = Parser.main Lexer.token @@ Lexing.from_string s
let a str tree = let _ = assert ((parse str)=tree) in ()

let chkpass str fs =
  let ast = parse str in
  List.iter (fun f -> f ast) fs

let chkfail str f =
  try  ignore @@ f @@ parse str
  with e -> print_endline @@ "encountered " ^ Printexc.to_string e

let print_tyenv env = print_endline @@ "[" ^ (String.concat "; " @@ List.map (fun (n, t) -> "(" ^ n ^ ", " ^ string_of_ty t ^ ")") env)

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
  a (func_s "a = 2 + 3;") (stat_v (Exp (SetEq("a", (InfixOper (Plus, IntLit 2, IntLit 3))))));
  a (func_s "a && 1 + 2 >3;")
    (exp_v (InfixOper (And, Var "a", (InfixOper (Gt, (InfixOper (Plus, IntLit 1, IntLit 2)), IntLit 3)))));
  a (func_s "'c';") (exp_v (CharLit 'c'));
  a "const a : int = 5; func b () : bool { 1; }"
    (Prog [Const (("a", Int), (IntLit 5)); Fun ("b", [], Bool, [Exp (IntLit 1)])]);
  a "const a : int = 5; func b () : bool { }"
    (Prog [Const (("a", Int), (IntLit 5)); Fun ("b", [], Bool, [])]);
  chkpass "func main () : void { let a : int = 5; a = 3; let b : int = 4; b = -2 * a; }"
          [constcheck; typecheck];
  chkpass "const a : int = 5; func b (a : int) : bool { return thing(5,3); }"
          [constcheck; typecheck];
  chkpass "func main () : bool { return voidf(); }"
          [constcheck; typecheck];
  chkpass "func main () : int { if (5 < 3) { return 12; } }"
          [constcheck; typecheck];
  chkpass "func main () : int { if (5 < 3) { return 12; } }"
          [constcheck; typecheck];
  chkpass "func main () : char { return 'h'; }"
          [typecheck];
  chkfail "func main () : int { if (5 < 3) { return true; } }"
          typecheck;
  chkfail "func main () : int { const a : int = 4; a = 3; }"
          constcheck;
  chkfail "func main () : int { a = 3; }"
          constcheck;
  chkfail "func main () : int { let a : int = 4; let a : int = 3; }"
          typecheck;
  )
