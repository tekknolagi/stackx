module AST = struct
  module PREV = L04controlflow.AST
  open VM

  type op = [
    | `Plus
    | `Minus
    | `Times
    | `Div
  ]

  type t = [
    PREV.t

    | `Literal of [ `Int of int ]
    | `Binop of (space * op * t * t)
  ]

  let lower (ast : t list) =
    let rec lower_one = function
      | `Literal (`Int i) ->
          [`Push (`Imm i)]
      | `Binop (dst, `Plus, e1, e2) ->
          lower_one e1
          @ lower_one e2
          @ [
            `Pop VM.tmp;
            `Add (`Deref (0, VM.sp), `Deref (0, VM.sp), VM.tmp);
            `Pop dst;
          ]
      | `Binop (dst, `Minus, e1, e2) ->
          lower_one e1
          @ lower_one e2
          @ [
            `Pop VM.tmp;
            `Sub (`Deref (0, VM.sp), `Deref (0, VM.sp), VM.tmp);
            `Pop dst;
          ]
      | `Binop (dst, `Times, e1, e2) ->
          lower_one e1
          @ lower_one e2
          @ [
            `Pop VM.tmp;
            `Mul (`Deref (0, VM.sp), `Deref (0, VM.sp), VM.tmp);
            `Pop dst;
          ]
      | `Binop (dst, `Div, e1, e2) ->
          lower_one e1
          @ lower_one e2
          @ [
            `Pop VM.tmp;
            `Div (`Deref (0, VM.sp), `Deref (0, VM.sp), VM.tmp);
            `Pop dst;
          ]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
