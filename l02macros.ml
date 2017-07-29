module AST = struct
  module PREV = L01substitutions.AST
  open VM

  type t = [
    PREV.t

    | `And of op3
    | `Or of op3
    | `Push of op1
    | `Pop of op1
    | `Call of op1
    | `Ret
  ]

  let rec lower (ast : t list) =
    let lower_one = function
      | `And (dst, a, b) -> [
        `Nand (dst, a, b);
        `Not (dst, dst)
      ]
      | `Or (dst, a, b) -> [
        `Nand (tmp, a, a);
        `Nand (dst, b, b);
        `Nand (dst, tmp, dst)
      ]
      | `Push a -> [
        `Move (`Deref (`Offset (0, VM.sp)), a);
        `Dec VM.sp
      ]
      | `Pop a -> [
        `Inc VM.sp;
        `Move (a, `Deref (`Offset (0, VM.sp)))
      ]
      | `Call addr -> lower [
        `Push (`Offset (1, VM.ip));
        `Br addr
      ]
      | `Ret -> lower [
        `Pop VM.ip
      ]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
