module AST = struct
  module PREV = L04controlflow.AST
  open VM

  type name = string
  type t = [
    PREV.t

    | `AsmFun of (name * t list)
    | `Funcall of (name * space list)
  ]

  let rec lower (ast : t list) =
    let lower_one = function
      | `AsmFun (lbl, body) ->
            [`Label lbl]
            @ lower body
      | `Funcall (lbl, args) ->
          (* Make a new stack frame *)
          [ `Push VM.bp;
            `Move (VM.bp, VM.sp);
          ]
          (* Push arguments in reverse order *)
          @ List.map (fun space -> `Push space) (List.rev args)
          @ [ `CallLabel (`Label lbl);
              (* Remove args from stack frame *)
              `Add (VM.sp, VM.sp, `Imm (List.length args));
            ]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
