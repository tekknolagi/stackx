module Typed_AST = struct
  type name = string
  type var = name * Ast.Type.t
  let string_of_var (n, t) = n ^ " : " ^ Ast.Type.to_string t

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

  type ty = Prim of Ast.Type.t | Arrow of Ast.Type.t list
  let string_of_ty = function
    | Prim t -> Ast.Type.to_string t
    | Arrow ts -> String.concat " -> " @@ List.map Ast.Type.to_string ts

  type exp = ty * Ast.AST.exp
  let string_of_exp (t, e) = Ast.AST.string_of_exp e ^ " : " ^ string_of_ty t

  type statement = ty * Ast.AST.statement
  type toplevel_def = ty * Ast.AST.toplevel_def
  type program = toplevel_def list
  type t = program

  exception Redefinition of string
  exception Unhandled

  type tyenv = string * ty list
  let check basis p =
    let open Ast.AST in
    let check_def def tyenv =
      match def with
      | Const ((n, t), e) ->
          if List.mem_assoc n tyenv then raise @@ Redefinition n
          else (n,Prim t)::tyenv
      | Fun _ -> raise Unhandled
    in
    match p with
    | Prog defs -> List.fold_right check_def defs basis
end

let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let () =
  let open Ast.Type in
  let open Typed_AST in
  let basis = [
    "*", Arrow [Int; Int; Int];
  ]
  in
  let prog = parse
  "const a : int = 5; func b (c : bool) : int { return 5; }"
  in
  let progty = check basis prog in
  print_endline @@ "[" ^ (String.concat "; " @@ List.map (fun (n, t) -> "(" ^ n ^ ", " ^ string_of_ty t ^ ")") progty)
