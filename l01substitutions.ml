module AST = struct
  module PREV = L00rabbit.AST
  open VM

  type t = [
    PREV.t

    | `Inc of op1
    | `Dec of op1
    | `Cmp of op2
    | `Not of op2
    | `Move of op2
  ]

  let lower (ast : t list) =
    let lower_one = function
      | `Inc a -> [`Add (a, a, `Imm 1)]
      | `Dec a -> [`Sub (a, a, `Imm 1)]
      | `Cmp (a, b) -> [`Sub (VM.tmp, a, b)]
      | `Not (dst, a) -> [`Nand (dst, a, a)]
      | `Move (dst, src) -> [`Add (dst, `Imm 0, src)]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
