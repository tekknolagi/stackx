module AST = struct
  module PREV = L04controlflow.AST
  open VM

  type name = string
  type t = [
    PREV.t

    | `Funcall of (name * space list)
  ]

  let lower (ast : t list) =
    let lower_one = function
      | `Funcall (lbl, args) ->
          List.map (fun space -> `Push space) args
          @ [`Goto (`Label lbl)]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
