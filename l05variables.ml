module AST = struct
  module PREV = L04controlflow.AST
  open VM

  type name = string
  type e = [
    | `Var of name
    | `Literal of int
    | `Binop of ([`Plus | `Minus] * t * t)
  ]
  and t = [
    PREV.t

    | `Let of (name * e)
  ]

  let vars = Counter.make "__tmp__"
  let lower (ast : t list) =
    let nextvar () = Counter.next vars in
    let lower_e = function
      | `Var n -> (n, [])
      | `Literal i ->
          let n = nextvar () in
          (n, [`Let (n
    let lower_one = function
      (* | `Let (n, e) ->
          lower_e e
          *)
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
