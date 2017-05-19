module Type = struct
  type t = Void | Int | String | Bool

  let to_string = function
    | Void -> "void"
    | Int -> "int"
    | String -> "string"
    | Bool -> "bool"
end

module AST = struct
  type name = string
  type var = name * Type.t
  let string_of_var (n, t) = n ^ " : " ^ Type.to_string t

  type mop =
    | Plus
    | Minus
    | Times
    | Div
  let string_of_mop = function
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"

  type cop =
    | Lt
    | Gt
    | Lte
    | Gte
    | Eq
    | And
    | Or
  let string_of_cop = function
    | Lt -> "<" | Gt -> ">"
    | Lte -> "<=" | Gte -> ">=" | Eq -> "=="
    | And -> "&&" | Or -> "||"

  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
    | PrefixOper of mop * exp
    | MathOper of mop * exp * exp
    | CompOper of cop * exp * exp
    | Funcall of name * exp list
  let rec string_of_exp = function
    | IntLit i -> string_of_int i
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | Var n -> n
    | PrefixOper (o, e) ->
        "(" ^ string_of_mop o ^ "(" ^ string_of_exp e ^ "))"
    | MathOper (o, e1, e2) ->
        "((" ^ string_of_exp e1 ^ ") "
        ^ string_of_mop o ^
        "(" ^ string_of_exp e2 ^ "))"
    | CompOper (o, e1, e2) ->
        "((" ^ string_of_exp e1 ^ ") "
        ^ string_of_cop o ^
        "(" ^ string_of_exp e2 ^ "))"
    | Funcall (n, es) ->
        n ^ "("
        ^ (String.concat "," @@ List.map string_of_exp es) ^ ")"

  type lettype =
    | LLet
    | LConst

  type statement =
    | Assignment of lettype * var * exp
    | SetEq of name * exp
    | If of exp * statement list
    | IfElse of exp * statement list * statement list
    | Return of exp
    | Exp of exp
  type toplevel_def =
    | Fun of name * var list * Type.t * statement list
    | Const of var * exp
  type program = Prog of toplevel_def list
  type t = program
end
