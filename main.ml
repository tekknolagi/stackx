let prog : L00machine.AST.t list =
  [
    `AsmFun ("addtwo", [
      `Add (VM.ret, `Deref (`Offset (1, VM.sp)), `Deref (`Offset (2, VM.sp)));
      `Ret;
    ]);

    `Label "_start";
    `Funcall ("addtwo", [`Imm 3; `Imm 4]);
    `Halt;
  ] |> L02funcall.AST.lower
    |> L01controlflow.AST.lower
;;

let () =
  List.iter (fun x -> print_endline @@ L00machine.AST.show x) prog
