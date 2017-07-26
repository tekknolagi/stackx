module AST = struct
  open VM

  type t = [
    L00rabbit.AST.t
    (*
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
    *)

    | `Cmp of op2
    | `Not of op2
    | `And of op3
    | `Push of op1
    | `Pop of op1
    | `Call of op1
    | `Ret
  ]

  let lower ast =
    let lower_one = function
      | `Cmp (a, b) -> [`Sub (VM.tmp, a, b)]
      | `Not (dst, a) -> [`Nand (dst, a, a)]
      | x -> [L00rabbit.AST.lower x]
    in
    List.concat @@ List.map lower_one ast
end
