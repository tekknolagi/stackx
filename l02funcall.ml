module AST = struct
  module PREV = L01controlflow.AST
  open VM

  type name = string
  type t = [
    PREV.t

    | `AsmFun of (name * t list)
    | `AsmFuncall of (name * space list)
  ]

  let rec lower (ast : t list) =
    let lower_one = function
      | `AsmFun (lbl, body) ->
            [`Label lbl]
            @ lower body
      | `AsmFuncall (lbl, args) ->
          (* Make a new stack frame *)
          [ `Push VM.bp;
            `Move (VM.bp, VM.sp);
          ]
          (* Push arguments in reverse order *)
          @ List.map (fun space -> `Push space) (List.rev args)
          @ [ `Call (`Label lbl);
              (* Remove args from stack frame *)
              `Add (VM.sp, VM.sp, `Imm (List.length args));
            ]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
