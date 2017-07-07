module Type = struct
  type prim = Unit | Int | String | Bool | Char

  let rec prim_to_string = function
    | Unit -> "()"
    | Int -> "Int"
    | String -> "String"
    | Bool -> "Bool"
    | Char -> "Char"

  type t = Prim of prim | Arrow of t list | Pointer of t
  let rec to_string = function
    | Prim t -> "Prim " ^ prim_to_string t
    | Arrow ts ->
        "Arrow [" ^ (String.concat "; " @@ List.map to_string ts) ^ "]"
    | Pointer t -> "Pointer (" ^ to_string t ^ ")"
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
    | UnitLit
    | CharLit of char
    | BoolLit of bool
    | Var of name
    | Ref of exp
    | Deref of exp
    | PrefixOper of op * exp
    | InfixOper of op * exp * exp
    | Funcall of exp * exp list
    | SetEq of name * exp
  let rec string_of_exp = function
    | IntLit i -> "IntLit " ^ string_of_int i
    | UnitLit -> "()"
    | CharLit c -> "CharLit " ^ String.make 1 c
    | BoolLit b -> "BoolLit " ^ string_of_bool b
    | Var n -> "Var \"" ^ n ^ "\""
    | Ref e -> "Ref (" ^ string_of_exp e ^ ")"
    | Deref e -> "Deref (" ^ string_of_exp e ^ ")"
    | PrefixOper (o, e) ->
        "(" ^ string_of_op o ^ "(" ^ string_of_exp e ^ "))"
    | InfixOper (o, e1, e2) ->
        "((" ^ string_of_exp e1 ^ ") "
        ^ string_of_op o ^
        " (" ^ string_of_exp e2 ^ "))"
    | Funcall (f, es) ->
        "Funcall (" ^ string_of_exp f ^ ", "
        ^ (String.concat "," @@ List.map string_of_exp es) ^ ")"
    | SetEq (n, e) -> "SetEq (" ^ n ^ ", " ^ string_of_exp e ^ ")"

  type lettype =
    | LLet
    | LConst
  let string_of_let = function | LLet -> "let" | LConst -> "const"

  type statement =
    | Let of lettype * var * exp
    | If of exp * statement list
    | IfElse of exp * statement list * statement list
    | While of exp * statement list
    | Return of exp
    | Exp of exp
  let rec string_of_statement = function
    | Let (lt, v, e) ->
        string_of_let lt ^ " " ^ string_of_var v ^ " = " ^ string_of_exp e ^ ";"
    | If (cond, iftrue) ->
        "if (" ^ string_of_exp cond ^ ") " ^ string_of_block iftrue
    | IfElse (cond, iftrue, iffalse) ->
        string_of_statement (If (cond, iftrue)) ^ " else " ^ string_of_block iffalse
    | While (e, b) -> "while (" ^ string_of_exp e ^ ")" ^ string_of_block b
    | Return e -> "return " ^ string_of_exp e ^ ";"
    | Exp e -> string_of_exp e ^ ";"
  and string_of_block block =
    "{\n" ^ (String.concat "\n" @@ List.map string_of_statement block) ^ "\n}"

  type toplevel_def =
    | Fun of name * var list * Type.t * statement list
    | Const of var * exp
  let string_of_toplevel_def = function
    | Fun (name, formals, ty, body) ->
        "func " ^ name ^ "(" ^ (String.concat ", " @@ List.map string_of_var
        formals) ^ ") : " ^ Type.to_string ty ^ "\n" ^ string_of_block body
    | Const (v, e) -> string_of_statement (Let (LConst, v, e))
  type program = Prog of toplevel_def list
  let string_of_program (Prog defs) =
    String.concat "\n" @@ List.map string_of_toplevel_def defs
  type t = program
end

let const_eval defs =
  let open AST in
  let binint = function
    | Plus -> (+)
    | Minus -> (-)
    | Times -> ( * )
    | Div -> (/)
    | _ -> failwith "can't happen 0"
  in
  let binbool = function
    | Lt -> (<)
    | Gt -> (>)
    | Lte -> (<=)
    | Gte -> (>=)
    | And -> (&&)
    | Or -> (||)
    | _ -> failwith "can't happen 1"
  in
  let rec evexp env = function
    | IntLit i -> IntLit i
    | UnitLit -> UnitLit
    | CharLit c -> CharLit c
    | BoolLit b -> BoolLit b
    | Var n -> Varenv.assoc n env
    | PrefixOper (Plus, e) -> evexp env e
    | PrefixOper (Minus, e) ->
        (match evexp env e with
         | IntLit i -> IntLit (0-i)
         | _ -> failwith "can't happen 2"
        )
    | PrefixOper (Not, e) ->
        (match evexp env e with
         | BoolLit b -> BoolLit (not b)
         | _ -> failwith "can't happen 3"
         )
    | PrefixOper _ -> failwith "unsupported"
    | InfixOper (op, e1, e2) ->
        (match (evexp env e1, evexp env e2) with
         | (IntLit i1, IntLit i2) -> IntLit ((binint op) i1 i2)
         | (BoolLit b1, BoolLit b2) -> BoolLit ((binbool op) b1 b2)
         | _ -> failwith "can't happen"
        )
    | Ref _ | Deref _ -> failwith "cannot ref/deref in toplevel const"
    | Funcall _ -> failwith "cannot funcall in toplevel const"
    | SetEq _ -> failwith "cannot seteq in toplevel const"
  in
  let evdef env = function
    | Const ((n,t), e) ->
        let v = evexp env e in
        (Varenv.bind n v env), Const ((n, t), v)
    | x -> env, x
  in
  let constenv =
    List.fold_left (fun env cur -> fst @@ evdef env cur) Varenv.empty defs
  in
  List.map (fun def -> snd @@ evdef constenv def) defs
