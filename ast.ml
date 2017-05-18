module Type = struct
  type t = Int | String

  let to_string = function
    | Int -> "Int"
    | String -> "String"
end

module AST = struct
  type name = string
  type var = name * Type.t
  let string_of_var (n, t) = n ^ " : " ^ Type.to_string t

  type op =
    | Plus
    | Minus
    | Times
    | Div
  let string_of_op = function
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
    | PrefixOper of op * exp
    | InfixOper of op * exp * exp
    | Funcall of name * exp list
  let rec string_of_exp = function
    | IntLit i -> string_of_int i
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | Var n -> n
    | PrefixOper (o, e) ->
        "(" ^ string_of_op o ^ "(" ^ string_of_exp e ^ "))"
    | InfixOper (o, e1, e2) ->
        "((" ^ string_of_exp e1 ^ ") "
        ^ string_of_op o ^
        "(" ^ string_of_exp e2 ^ "))"
    | Funcall (n, es) ->
        n ^ "("
        ^ (String.concat "," @@ List.map string_of_exp es) ^ ")"

  type lettype =
    | LLet
    | LConst

  type statement =
    | Assignment of lettype * var * exp
    | If of exp * statement list
    | IfElse of exp * statement list * statement list
    | Return of exp
    | Exp of exp
  type function_def = Fun of name * var list * Type.t * statement list
  type program = Prog of function_def list
  type t = program
end
