let prog = L02macros.AST.lower([
  `In (`Reg 1);
  `In (`Reg 2);
  `Inc (`Reg 2);
  `Cmp ((`Reg 1), (`Reg 2));
  `Out (`Reg 1)
])
