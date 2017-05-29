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

  type ty = Prim of Ast.Type.t | Arrow of ty list
  let rec string_of_ty = function
    | Prim t -> Ast.Type.to_string t
    | Arrow ts ->
        String.concat " -> " @@ List.map string_of_ty ((Prim Ast.Type.Void)::ts)

  type exp = ty * Ast.AST.exp
  let string_of_exp (t, e) = Ast.AST.string_of_exp e ^ " : " ^ string_of_ty t

  type statement = ty * Ast.AST.statement
  type toplevel_def = ty * Ast.AST.toplevel_def
  type program = toplevel_def list
  type t = program

  exception Redefinition of string
  exception TypeMismatch of string
  exception Unhandled

  type tyenv = string * ty list
  let check basis p =
    let open Ast.AST in
    let type_of tyenv e =
      let rec tyApply formals actuals =
        match (formals, actuals) with
        | (Arrow [_], [_]) -> raise @@ TypeMismatch "too many arguments"
        | (Arrow (f::restFormals), a::restActuals) ->
            if a=f then tyApply (Arrow restFormals) restActuals
            else raise @@ TypeMismatch ("mismatch in function call. expected "
                                        ^ string_of_ty f ^ " but got "
                                        ^ string_of_ty a)
        | (Arrow [t], []) -> t
        | (Arrow _, ls) -> raise @@ TypeMismatch "too many arguments"
        | (Prim _, _) -> raise @@ TypeMismatch "non-function variable called as function"
      in
      let rec ty = function
      | IntLit _ -> Prim Ast.Type.Int
      | CharLit _ -> Prim Ast.Type.Char
      | Var n -> List.assoc n tyenv
      | PrefixOper (_, e) ->
          (match ty e with
          | Prim Ast.Type.Int -> Prim Ast.Type.Int
          | _ -> raise @@ TypeMismatch "PrefixOpers expect int")
      | MathOper (_, e1, e2) ->
          (match (ty e1, ty e2) with
          | (Prim Ast.Type.Int, Prim Ast.Type.Int) -> Prim Ast.Type.Int
          | _ -> raise @@ TypeMismatch "MathOpers expect ints")
      | CompOper (c, e1, e2) when c=And || c=Or ->
          (match (ty e1, ty e2) with
          | (Prim Ast.Type.Bool, Prim Ast.Type.Bool) -> Prim Ast.Type.Bool
          | _ -> raise @@ TypeMismatch "And/Or expect 2 bools")
      | CompOper (_, e1, e2) ->
          (match (ty e1, ty e2) with
          | (Prim Ast.Type.Int, Prim Ast.Type.Int) -> Prim Ast.Type.Bool
          | _ -> raise @@ TypeMismatch "ComparisonOps expect ints")
      | Funcall (f, actuals) ->
          tyApply (ty @@ Var f) (List.map ty actuals)
      in ty e
    in
    let check_statement t statement tyenv =
      match statement with
      | Return e ->
         (match type_of tyenv e with
         | t' when t'=t -> tyenv
         | t' -> raise @@ TypeMismatch ("returned value's type must match fn "
                                        ^ "type. found " ^ string_of_ty t'
                                        ^ " but expected " ^ string_of_ty t)
         )
      | _ -> raise Unhandled
    in
    let check_fun t body tyenv =
      ignore @@ List.fold_right (check_statement t) body tyenv
    in
    let check_def def tyenv =
      match def with
      | Const ((n, t), e) ->
          if List.mem_assoc n tyenv then raise @@ Redefinition n
          else (n,Prim t)::tyenv
      | Fun (n, formals, t, body) ->
          let newenv = (List.map (fun (n, t) -> (n, Prim t)) formals)@tyenv in
          let () = check_fun (Prim t) body newenv in
          (n, Arrow ((List.map (fun (n,t) -> Prim t) formals) @ [Prim t]))::tyenv
    in
    match p with
    | Prog defs -> List.fold_right check_def defs basis
end

let parse s = Parser.main Lexer.token @@ Lexing.from_string s

let () =
  let open Ast.Type in
  let open Typed_AST in
  let basis = [
    "*", Arrow [Prim Int; Prim Int; Prim Int];
    "thing", Arrow [Prim Int; Prim Int; Prim Bool];
  ]
  in
  let prog = parse
  "const a : int = 5; func b (a : int) : bool { return thing(5,3); }"
  in
  let progty = check basis prog in
  print_endline @@ "[" ^ (String.concat "; " @@ List.map (fun (n, t) -> "(" ^ n ^ ", " ^ string_of_ty t ^ ")") progty)
