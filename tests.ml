open Ast.AST
open Ast.Type
open Typed_ast.Typed_AST

let parse s = Parser.main Lexer.token @@ Lexing.from_string s
let a str tree =
  try
    let _ = assert ((parse str)=tree) in print_endline "test passed"
  with | Parser.Error -> prerr_endline @@ "PARSE ERROR IN:\n    " ^ str

let chkpass str fs =
  let ast = parse str in
  List.iter (fun f -> f ast) fs

exception ExpectedFailure of string

let chkfail str f =
  try  (ignore @@ f @@ parse str;
        raise @@ ExpectedFailure str)
  with e -> print_endline @@ "encountered " ^ Printexc.to_string e

let print_tyenv env = print_endline @@ "[" ^ (String.concat "; " @@ List.map (fun (n, t) -> "(" ^ n ^ ", " ^ to_string t ^ ")") env)

let func_s s = "func a() : int { " ^ s ^ "}"
let stat_v s = Prog [Fun ("a", [], Prim Int, [s])]
let exp_v e = stat_v (Exp e)

let expressions = [
  "1 + 2 - 3"      , InfixOper (Minus, InfixOper (Plus, IntLit 1, IntLit 2), IntLit 3);
   "hello()"       , Funcall (Var "hello", []);
   "hello(1)"      , Funcall (Var "hello", [IntLit 1]);
   "hello(1, 2, 3)", Funcall (Var "hello", [IntLit 1; IntLit 2; IntLit 3]);
   "hello(a(1))"   , Funcall (Var "hello", [Funcall (Var "a", [IntLit 1])]);
   "1 < 3"         , InfixOper (Lt, IntLit 1, IntLit 3);
   "a + b"         , InfixOper (Plus, Var "a", Var "b");
   "'c'"           , CharLit 'c';
   "3()"           , Funcall (IntLit 3, []);
  "a && 1 + 2 >3"  , InfixOper (And, Var "a", (InfixOper (Gt, (InfixOper (Plus, IntLit 1, IntLit 2)), IntLit 3)));
]

let statements = [
  "let a : int = 4;"         , Let (LLet, ("a", Prim Int), (IntLit 4));
  "const a : int = 4;"       , Let (LConst, ("a", Prim Int), (IntLit 4));
  "if (3) { 4; }"            , If (IntLit 3, [Exp (IntLit 4)]);
  "if (3) { 4; } else { 5; }", IfElse (IntLit 3, [Exp (IntLit 4)], [Exp (IntLit 5)]);
  "return 10;"               , Return (IntLit 10);
  "return a(1);"             , Return (Funcall (Var "a", [IntLit 1]));
  "a = 2 + 3;"               , Exp (SetEq ("a", (InfixOper (Plus, IntLit 2, IntLit 3))));
  "let a : int * = &3;"      , Let (LLet, ("a", Pointer (Prim Int)), (Ref (IntLit 3)));
  "let a : int * = 3;"       , Let (LLet, ("a", Pointer (Prim Int)), (IntLit 3));
]

let programs = [
  "const a : int = 5; func b () : bool { 1; }",
    Prog [Const (("a", Prim Int), (IntLit 5)); Fun ("b", [], Prim Bool, [Exp (IntLit 1)])];
  "const a : int = 5; func b () : bool { }",
    Prog [Const (("a", Prim Int), (IntLit 5)); Fun ("b", [], Prim Bool, [])];
]

let () = (
  List.iter (fun (s, e) -> a (func_s @@ s ^ ";") (exp_v e)) expressions;
  List.iter (fun (s, st) -> a (func_s s) (stat_v st)) statements;
  List.iter (fun (s, p) -> a s p) programs;
  chkpass "func main () : void { let a : int = 5; a = 3; let b : int = 4; b = -2 * a; }"
          [constcheck; typecheck];
  chkpass "const a : int = 5; func b (a : int) : bool { return thing(5,3); }"
          [constcheck; typecheck];
  chkpass "func main () : bool { return voidf(); }"
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
  chkfail "func f () : int { return 3; } func main () : int { f(3); }"
          typecheck;
  chkfail "func f (a : int) : int { return 3; } func main () : int { f(); }"
          typecheck;
  chkfail (func_s "let a : int * = 3;") typecheck;
  chkpass (func_s "let a : int * = &3;") [typecheck];
  )
