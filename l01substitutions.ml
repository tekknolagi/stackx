module PREV = L00rabbit.AST

module AST = struct
  open VM

  type t = [
    PREV.t

    | `Inc of op1
    | `Dec of op1
    | `Cmp of op2
    | `Not of op2
  ]

  let lower ast =
    let lower_one = function
      | `Inc a -> [`Add (a, a, `Imm 1)]
      | `Dec a -> [`Sub (a, a, `Imm 1)]
      | `Cmp (a, b) -> [`Sub (VM.tmp, a, b)]
      | `Not (dst, a) -> [`Nand (dst, a, a)]
      | x -> PREV.lower [x]
    in
    List.concat @@ List.map lower_one ast
end
