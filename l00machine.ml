module AST = struct
  open VM

  type 'a parametrized = [
    | `Label of string
    | `Halt
    | `Add of op3
    | `Sub of op3
    | `Mul of op3
    | `Div of op3
    | `Shr of op3
    | `Shl of op3
    | `Nand of op2
    | `And of op2
    | `Or of op2
    | `Xor of op3
    | `Br of op1
    | `Brz of op1
    | `Brnz of op1
    | `In of op1
    | `Out of op1
    | `Test of op1
    | `Inc of op1
    | `Dec of op1
    | `Move of op2
    | `Push of op1
    | `Pop of op1
    | `J of op1
    | `Jz of op1
    | `Jnz of op1
    | `Cmp of op2
    | `Call of 'a
    | `Ret
  ]
  type t = op1 parametrized

  let show : t -> string = function
    | `Halt -> "mov ebx, 0\nmov eax, 1\nint 0x80"
    | `Add o3 -> "add " ^ show_op3 o3
    | `Sub o3 -> "sub " ^ show_op3 o3
    | `Mul o3 -> "mul " ^ show_op3 o3
    | `Div o3 -> "div " ^ show_op3 o3
    | `Shr o3 -> "shr " ^ show_op3 o3
    | `Shl o3 -> "shl " ^ show_op3 o3
    | `Nand o2 -> "nand " ^ show_op2 o2
    | `Xor o3 -> "xor " ^ show_op3 o3
    | `Br o1 -> "br " ^ show_op1 o1
    | `Brz o1 -> "brz " ^ show_op1 o1
    | `Brnz o1 -> "brnz " ^ show_op1 o1
    | `In o1 -> "in " ^ show_op1 o1
    | `Out o1 -> "out " ^ show_op1 o1
    | `Test o1 -> "test " ^ show_op1 o1
    | `Dec o1 -> "dec " ^ show_op1 o1
    | `Inc o1 -> "inc " ^ show_op1 o1
    | `J o1 -> "j " ^ show_op1 o1
    | `Jz o1 -> "jz " ^ show_op1 o1
    | `Jnz o1 -> "jnz " ^ show_op1 o1
    | `Push o1 -> "push " ^ show_op1 o1
    | `Pop o1 -> "pop " ^ show_op1 o1
    | `And o2 -> "and " ^ show_op2 o2
    | `Or o2 -> "or " ^ show_op2 o2
    | `Label l -> l ^ ":"
    | `Ret -> "ret"
    | `Move o2 -> "mov " ^ show_op2 o2
    | `Cmp o2 -> "cmp " ^ show_op2 o2
    | `Call o1 -> "call " ^ show_op1 o1

  let lower (ast : t list) = ast
end
