let prog : L00rabbit.AST.t list = L03labels.AST.lower [
  `Label "start";
  `In (`Reg 1);
  `In (`Reg 2);
  `Label "postoutput";
  `Inc (`Reg 2);
  `Cmp ((`Reg 1), (`Reg 2));
  `Out (`Reg 1)
]
