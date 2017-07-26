module AST = struct
  type reg = int
  type arg = [ `Reg of reg | `Immed of int ]
  type space = [ `Arg of arg | `Deref of arg ]
  type op1 = space
  type op2 = space * space
  type op3 = space * space * space
  type t = [
    | `Halt
    | `Move of op2
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
  ]

  let lower ast = ast
end
