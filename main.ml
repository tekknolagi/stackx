let prog = L01macros.([
  `In (`Arg (`Reg 1));
  `In (`Arg (`Reg 2));
  `Cmp (`Arg (`Reg 1), `Arg (`Reg 2));
  `Out (`Arg (`Reg 1))
])
