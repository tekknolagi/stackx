module AST = struct
  open VM

  type t = [
    L00rabbit.AST.t

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
      | x -> L00rabbit.AST.lower [x]
    in
    List.concat @@ List.map lower_one ast
end
