let prog : L00rabbit.AST.t list =
  [
    `Label "start";
    `In (`Reg 1);
    `In (`Reg 2);
    `Cmp ((`Reg 1), (`Reg 2));
    `Ifz (
      [`Out (`Imm 97)],
      [`Out (`Imm 98)]
    );
    `Inc (`Reg 2);
    `Out (`Reg 1)
  ] |> L05expressions.AST.lower
    (* |> L05variables.AST.lower *)
    |> L04controlflow.AST.lower
    |> L03labels.AST.lower
    |> L02macros.AST.lower
    |> L01substitutions.AST.lower
;;

let () =
  List.iter (fun x -> print_endline @@ L00rabbit.AST.show x) prog
