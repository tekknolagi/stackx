module Type = struct
  type t = Int | String

  let to_string = function
    | Int -> "Int"
    | String -> "String"
end

module AST = struct
  type name = string
  type var = name * Type.t
  type op =
    | Plus
    | Minus
    | Times
    | Div
  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
    | InfixOper of op * exp * exp
    | Funcall of name * exp list
  type statement =
    | Assignment of var * exp
    | If of exp * statement list * statement list
    | Return of exp
    | Exp of exp
  type function_def = Fun of name * var list * Type.t * statement list
  type program = Prog of function_def list
  type t = program
end
