module Printer = struct
  open Ast
  open Ast.TypedAst

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
end
