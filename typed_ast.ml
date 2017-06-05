module Typed_AST = struct
  type name = string
  type var = name * Ast.Type.t
  let string_of_var (n, t) = n ^ " : " ^ Ast.Type.to_string t

  type exp = Ast.Type.t * Ast.AST.exp
  let string_of_exp (t, e) = Ast.AST.string_of_exp e ^ " : " ^ Ast.Type.to_string t

  type statement = Ast.Type.t * Ast.AST.statement
  type toplevel_def = Ast.Type.t * Ast.AST.toplevel_def
  type program = toplevel_def list
  type t = program

  exception Redefinition of string
  exception TypeMismatch of string
  exception UnboundVariable of string
  exception Unhandled

  type tyenv = string * Ast.Type.t list
  let typecheck p =
    let open Ast.Type in
    let open Ast.AST in
    let type_of tyenv e =
      let rec tyApply formals actuals =
        match (formals, actuals) with
        | (Arrow [_], [_]) -> raise @@ TypeMismatch "too many arguments"
        | (Arrow (f::restFormals), a::restActuals) when a=f ->
            tyApply (Arrow restFormals) restActuals
        | (Arrow (f::_), a::_) ->
            raise @@ TypeMismatch ("mismatch in function call. expected "
                                   ^ to_string f ^ " but got "
                                   ^ to_string a)
        | (Arrow [t], []) -> t
        | (Arrow _, ls) -> raise @@ TypeMismatch "too few arguments"
        | (Prim _, _) -> raise @@ TypeMismatch "non-function variable called as function"
        | (Pointer _, _) -> raise @@ TypeMismatch "non-function variable called as function"
        (* | (Pointer (Arrow formals), actuals) -> tyapply  *)
      in
      let rec ty = function
      | IntLit _ -> Prim Ast.Type.Int
      | CharLit _ -> Prim Ast.Type.Char
      | Var n -> Varenv.assoc n tyenv
      | Ref e -> Pointer (ty e)
      | Deref e ->
          (match ty e with
          | Pointer t -> t
          | _ -> raise @@ TypeMismatch "cannot deref non-pointer")
      | PrefixOper (o, e) ->
          let tyO = ty @@ Var ("u" ^ string_of_op o) in
          tyApply tyO [ty e]
      | InfixOper (o, e1, e2) ->
          let tyO = ty @@ Var (string_of_op o) in
          tyApply tyO [ty e1; ty e2]
      | Funcall (f, actuals) ->
          tyApply (ty @@ f) (List.map ty actuals)
      (* Will exist; checked earlier in pipeline. *)
      | SetEq (n, e) ->
          (match (ty (Var n), ty e) with
          | (tyN, tyE) when tyN=tyE -> tyE
          | _ -> raise @@ TypeMismatch ("assignment changes type of variable " ^ n))
      in ty e
    in
    let rec check_statement t tyenv stmt =
      match stmt with
      | Return e ->
         (match type_of tyenv e with
         | t' when t'=t -> tyenv
         | t' -> raise @@ TypeMismatch ("returned value's type must match fn "
                                        ^ "type. found " ^ to_string t'
                                        ^ " but expected " ^ to_string t)
         )
      | Let (_, (n, t), e) when Varenv.exists_curframe n tyenv -> raise @@ Redefinition n
      | Let (_, (n, t), e) ->
          (match type_of tyenv e with
          | tyE when tyE=t -> Varenv.bind n t tyenv
          | tyE ->
              raise @@ TypeMismatch ("variable assignment type mismatch. found "
                                     ^ to_string tyE ^ " but expected "
                                     ^ to_string t))
      | IfElse (cond, iftrue, iffalse) ->
          (match type_of tyenv cond with
          | Prim Ast.Type.Bool ->
              (ignore @@ List.fold_left (check_statement t) tyenv iftrue;
               ignore @@ List.fold_left (check_statement t) tyenv iffalse;
               tyenv)
          | _ -> raise @@ TypeMismatch "if condition must have type bool")
      | If (cond, iftrue) -> check_statement t tyenv (IfElse (cond, iftrue, []))
      | Exp e -> (ignore @@ type_of tyenv e; tyenv)
    in
    let check_fun t body tyenv =
      ignore @@ List.fold_left (check_statement t) tyenv body
    in
    let check_def tyenv = function
      | Const ((n, t), e) ->
          if Varenv.exists n tyenv then raise @@ Redefinition n
          else Varenv.bind n t tyenv
      | Fun (n, formals, t, body) ->
          let newenv = (List.map (fun (n, t) -> (n, t)) formals)::tyenv in
          let () = check_fun t body newenv in
          Varenv.bind n (Arrow ((List.map snd formals) @ [t])) tyenv
    in
    let basis = Ast.Type.([[
      "true", Prim Bool;
      "false", Prim Bool;
      "+", Arrow [Prim Int; Prim Int; Prim Int];
      "-", Arrow [Prim Int; Prim Int; Prim Int];
      "u-", Arrow [Prim Int; Prim Int];
      "*", Arrow [Prim Int; Prim Int; Prim Int];
      "/", Arrow [Prim Int; Prim Int; Prim Int];
      "u!", Arrow [Prim Bool; Prim Bool];
      "==", Arrow [Prim Int; Prim Int; Prim Bool];
      "<",  Arrow [Prim Int; Prim Int; Prim Bool];
      "<=", Arrow [Prim Int; Prim Int; Prim Bool];
      ">",  Arrow [Prim Int; Prim Int; Prim Bool];
      ">=", Arrow [Prim Int; Prim Int; Prim Bool];
      "&&", Arrow [Prim Bool; Prim Bool; Prim Bool];
      "||", Arrow [Prim Bool; Prim Bool; Prim Bool];
      "thing", Arrow [Prim Int; Prim Int; Prim Bool];
      "voidf", Arrow [Prim Bool]
    ]])
    in
    match p with
    | Prog defs -> ignore @@ List.fold_left check_def basis defs

  exception SettingConst of string

  let constcheck p =
    let open Ast.AST in
    let rec check_statement env = function
      | Let (LConst, (n, _), _) -> Varenv.bind n `Const env
      | Let (LLet, (n, _), _) -> Varenv.bind n `Mut env
      | IfElse (cond, iftrue, iffalse) ->
          (ignore @@ List.fold_left check_statement env iftrue;
           ignore @@ List.fold_left check_statement env iftrue;
           env)
      | If (cond, iftrue) -> check_statement env (IfElse (cond, iftrue, []))
      | Exp (SetEq (n, _)) when Varenv.unbound n env -> raise @@ UnboundVariable n
      | Exp (SetEq (n, _)) when `Const=Varenv.assoc n env -> raise @@ SettingConst n
      | Exp (SetEq (n, _)) when `Mut=Varenv.assoc n env -> env
      | Exp (SetEq (_, e)) -> check_statement env (Exp e)
      | _ -> env
    in
    let check_fun body env = List.fold_left check_statement env body in
    let check_def env = function
      | Const ((n, _), _) -> Varenv.bind n `Const env
      | Fun (n, _, _, body) ->
          let () = ignore @@ check_fun body env in
          Varenv.bind n `Const env
    in
    match p with
    | Prog defs -> ignore @@ List.fold_left check_def Varenv.empty defs
end
