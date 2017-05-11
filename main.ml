open Ast
open Printer

let () =
  let open TypedAst in
  let prog = Prog
    [
      Fun ("add2", ["a", Type.Int], Type.Int, [
        Return (InfixOper (Plus, Var "a", IntLit 2))
      ]);
      Fun ("main", [], Type.Int, [
        Return (Funcall ("add2", [IntLit 0]))
      ])
    ] in
  print_endline @@ Printer.to_string prog
