type op = [ `Plus | `Minus ]
type exp = [
  | `Lit of int
  | `InfixOper of op * exp * exp
  | `PrefixOper of [`Minus] * exp
]

let rec transform = function
  | `Lit i -> `Lit i
  | `InfixOper (o, e1, e2) -> `InfixOper (o, transform e1, transform e2)
  | `PrefixOper (o, e) -> `InfixOper (o, `Lit 0, transform e)

let rec eval = function
  | `Lit i -> i
  | `InfixOper (`Plus, e1, e2) -> (eval e1) + (eval e2)
  | `InfixOper (`Minus, e1, e2) -> (eval e1) - (eval e2)

let () =
  print_endline @@ string_of_int @@ eval @@ transform @@ `PrefixOper (`Minus, `Lit 4)
