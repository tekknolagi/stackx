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

  let show = function
    | `Halt -> "halt"
    | `Add o3 -> "add " ^ show_op3 o3
    | `Sub o3 -> "sub " ^ show_op3 o3
    | `Mul o3 -> "mul " ^ show_op3 o3
    | `Div o3 -> "div " ^ show_op3 o3
    | `Shr o3 -> "shr " ^ show_op3 o3
    | `Shl o3 -> "shl " ^ show_op3 o3
    | `Nand o3 -> "nand " ^ show_op3 o3
    | `Xor o3 -> "xor " ^ show_op3 o3
    | `Br o1 -> "br " ^ show_op1 o1
    | `Brz o1 -> "brz " ^ show_op1 o1
    | `Brnz o1 -> "brnz " ^ show_op1 o1
    | `In o1 -> "in " ^ show_op1 o1
    | `Out o1 -> "out " ^ show_op1 o1
    | `Test o1 -> "test " ^ show_op1 o1
    | `Malloc o2 -> "malloc " ^ show_op2 o2
    | `Free o1 -> "free " ^ show_op1 o1
    | _ -> "unknown"

  let lower (ast : t list) = ast
end
