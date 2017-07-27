let prog : L00rabbit.AST.t list =
  L01substitutions.AST.lower @@
  L02macros.AST.lower @@
  L03labels.AST.lower @@
  L04controlflow.AST.lower @@
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
  ]
