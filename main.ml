let prog : L00rabbit.AST.t list =
  [
    `AsmFun ("addtwo", [
      `Add (VM.ret, `Deref (`Offset (1, VM.sp)), `Deref (`Offset (2, VM.sp)));
      `Ret;
    ]);

    `Label "_start";
    `Funcall ("addtwo", [`Imm 3; `Imm 4]);
    `Halt;
  ] |> L05funcall.AST.lower
    |> L04controlflow.AST.lower
    |> L03labels.AST.lower
    |> L02macros.AST.lower
    |> L01substitutions.AST.lower
;;

let () =
  List.iter (fun x -> print_endline @@ L00rabbit.AST.show x) prog
