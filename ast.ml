module Type = struct
  type prim = Void | Int | String | Bool | Char

  let rec prim_to_string = function
    | Void -> "void"
    | Int -> "int"
    | String -> "string"
    | Bool -> "bool"
    | Char -> "char"

  type t = Prim of prim | Arrow of t list | Pointer of t
  let rec to_string ty =
    let s ts = String.concat " -> " @@ List.map to_string ts in
    match ty with
    | Prim t -> prim_to_string t
    | Arrow [x] -> s [Prim Void; x]
    | Arrow ts -> s ts
    | Pointer t -> to_string t ^ "*"
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
    | Lt
    | Gt
    | Lte
    | Gte
    | Eq
    | Not
    | And
    | Or
  let string_of_op = function
    | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/"
    | Lt -> "<" | Gt -> ">"
    | Lte -> "<=" | Gte -> ">=" | Eq -> "=="
    | Not -> "!"
    | And -> "&&" | Or -> "||"

  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
    | Ref of exp
    | Deref of exp
    | PrefixOper of op * exp
    | InfixOper of op * exp * exp
    | Funcall of exp * exp list
    | SetEq of name * exp
  let rec string_of_exp = function
    | IntLit i -> string_of_int i
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | Var n -> n
    | Ref e -> "(&(" ^ string_of_exp e ^ "))"
    | Deref e -> "(*(" ^ string_of_exp e ^ "))"
    | PrefixOper (o, e) ->
        "(" ^ string_of_op o ^ "(" ^ string_of_exp e ^ "))"
    | InfixOper (o, e1, e2) ->
        "((" ^ string_of_exp e1 ^ ") "
        ^ string_of_op o ^
        "(" ^ string_of_exp e2 ^ "))"
    | Funcall (n, es) ->
        string_of_exp n ^ "("
        ^ (String.concat "," @@ List.map string_of_exp es) ^ ")"
    | SetEq (n, e) -> n ^ " = " ^ string_of_exp e

  type lettype =
    | LLet
    | LConst

  type statement =
    | Let of lettype * var * exp
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
