module AST = struct
  module PREV = L01controlflow.AST
  open VM

  type name = string
  type t = [
    PREV.t

    | `AsmFun of (name * PREV.t list)
    | `VarDeclare of name
  ]

  let varmaker = Counter.make "__var"
  let nextvar () = Counter.next varmaker
  let rec lower (ast : t list) =
    let _make_unique name2rand = function
      | `VarDeclare n -> 3
    in
    let lower_one = function
      | `AsmFun (lbl, body) -> [`Label lbl] @ body
      | `VarDeclare n -> [
        (* ??? *)

      ]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast

end
