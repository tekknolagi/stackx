module Type = struct
  type t = Int | String

  let to_string = function
    | Int -> "Int"
    | String -> "String"
end

module AnnotationCons (A : sig type t end) (B : sig type t end) = struct
  type t = A.t * B.t
end

module Location = struct
  type t = int * int
end

module AST (T : sig type t end) = struct
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

  let rec var_to_string (n, t) =
    n ^ " : " ^ Type.to_string t

  and op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"

  and exp_to_string = function
    | IntLit i -> string_of_int i
    | CharLit c -> "'" ^ String.make 1 c ^ "'"
    | Var n -> n
    | InfixOper (op, e1, e2) -> "(" ^ exp_to_string e1 ^ " "
                                ^ op_to_string op ^ " " ^ exp_to_string e2 ^ ")"
    | Funcall (n, es) -> n ^ "(" ^ (String.concat ", " @@ List.map exp_to_string es) ^ ")"

  and statement_to_string s =
    let tos = function
    | Assignment (var, e) -> "let " ^ var_to_string var ^ " = "
                             ^ exp_to_string e
    | Return e -> "return " ^ exp_to_string e
    | If (cond, iftrue, iffalse) -> "<if-statement>"
    | Exp e -> exp_to_string e
    in tos s ^ ";"

  and function_def_to_string = function
    | Fun (name, vars, rettype, body) ->
        "func " ^ name ^ " (" ^ (String.concat " " @@ List.map var_to_string vars)
        ^ ") : " ^ Type.to_string rettype ^ " {\n" ^ (String.concat ";\n" @@
        List.map statement_to_string body) ^ "\n}"

  and to_string = function
    | Prog (fndefs) -> String.concat "\n\n" @@ List.map function_def_to_string fndefs
  type t = T.t * program
end

module TypedAst = AST(AnnotationCons(Type)(Location))

let () =
  let open TypedAst in
  let prog = Prog
    [
      Fun ("add2", ["a", Type.Int], Type.Int, [
        Return (InfixOper (Plus, Var "a", IntLit 2))
      ]);
      Fun ("main", [], Type.Int, [
        Return (Funcall ("add2", [IntLit 0]))
      ])
    ] in
  print_endline @@ to_string prog
