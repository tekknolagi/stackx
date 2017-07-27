module AST = struct
  open VM

  type t = [
    | `Halt
    | `Add of op3
    | `Sub of op3
    | `Mul of op3
    | `Div of op3
    | `Shr of op3
    | `Shl of op3
    | `Nand of op3
    | `Xor of op3
    | `Br of op1
    | `Brz of op1
    | `Brnz of op1
    | `In of op1
    | `Out of op1
    | `Test of op1
    | `Malloc of op2
    | `Free of op1
  ]

  let lower (ast : t list) = ast
end
