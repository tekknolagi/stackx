module AST1 = struct
  type op3 = int * int * int
  type op2 = int * int
  type op1 = int
  type command =
    | CMOV of op3
    | SLOAD of op3
    | SSTORE of op3
    | ADD of op3
    | MULT of op3
    | DIV of op3
    | NAND of op3
    | HALT
    | MAP of op2
    | UNMAP of op1
    | OUTPUT of op1
    | INPUT of op1
    | LOADP of op2
    | LOADV of op2
end
