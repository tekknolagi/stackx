let prog : L00machine.AST.t list =
  [
    `AsmFun ("addtwo", [
      `Add (VM.eax, `Deref (`Offset (1, VM.sp)), `Deref (`Offset (2, VM.sp)));
      `Ret;
    ]);

    `Label "_start";
    `AsmFuncall ("addtwo", [`Imm 3; `Imm 4]);
    `Halt;
  ] |> L03fundef.AST.lower
    |> L02funcall.AST.lower
    |> L01controlflow.AST.lower
;;

let () =
  List.iter (fun x -> print_endline @@ L00machine.AST.show x) prog
