module AST0 = struct
  type lit = IntLit of int | CharLit of char | BoolLit of bool
  let string_of_lit = function
    | IntLit i -> string_of_int i ^ "i"
    | CharLit c -> "'" ^ Char.escaped c ^ "'"
    | BoolLit b -> string_of_bool b
  type binop = Ast.AST.op
  type unop = [ `Not ]
  let string_of_unop = function
    | `Not -> "not"
  type var = V of int
  let string_of_var (V i) = "v" ^ string_of_int i
  type callable = Name of string | Address of var
  let string_of_callable = function
    | Name n -> n
    | Address r -> "(" ^ string_of_var r ^ ")"
  type command =
    | Load of var * lit
    | Binop of var * binop * var * var
    | Unop of var * unop * var
    | Mov of var * var
    | If of var * command list * command list
    | Call of callable
    | While of var * command list * command list
    | Push of var (* Push a variable onto the global stack. *)
    | Pop of var  (* Pop a value from the global stack into a variable. *)
    | Output of var
    | Input of var
    | Label of string

  let set r = string_of_var r ^ " <- "
  let rec string_of_command = function
    | Load (dst, l) ->
        set dst ^ string_of_lit l
    | Binop (dst, o, r1, r2)  ->
        set dst ^ string_of_var r1 ^ Ast.AST.string_of_op o ^ string_of_var r2
    | Unop (dst, o, r) ->
        set dst ^ string_of_unop o ^ string_of_var r
    | Mov (dst, src) ->
        set dst ^ string_of_var src
    | If (cond, iftrue, iffalse) ->
        "if (" ^ string_of_var cond ^ ") {\n" ^ String.concat "\n" (List.map
        string_of_command iftrue) ^ "\n} else {\n" ^ String.concat "\n" (List.map
        string_of_command iffalse) ^ "\n}"
    | Call (f) ->
        "call " ^ string_of_callable f
    | While (cr, cond, body) ->
        "while (" ^ string_of_var cr ^ ") {\n" ^ String.concat "\n" (List.map
        string_of_command cond) ^ "\n} | {\n" ^ String.concat "\n" (List.map
        string_of_command body) ^ "\n}"
    | Push r -> "push " ^ string_of_var r
    | Pop r -> "pop " ^ string_of_var r
    | Label n -> n ^ ":"
    | Output r -> "out(" ^ string_of_var r ^ ")"
    | Input dst -> set dst ^ "in()"

  let string_of_program p = String.concat "\n" (List.map string_of_command p)

  (* 0 for zero variable, 1 for global stack, 2 for stack ptr,
     3 for register allocation spill *)
  let num = ref 4
  let next _ = let i = !num in let () = num := !num + 1 in V i

  open Ast.AST

  let rec lower_exp exp env =
    let lower e = lower_exp e env in
    let zr = V 0 in
    match exp with
    | IntLit i ->
        let r = next () in
        r, [ Load (r, IntLit i) ]
    | CharLit c ->
        let r = next () in
        r, [ Load (r, CharLit c) ]
    | BoolLit b ->
        let r = next () in
        r, [ Load (r, BoolLit b) ]
    | Var n ->
        Varenv.assoc n env, []
    | InfixOper (Gte, e1, e2) ->
        lower_exp (InfixOper (Or, (InfixOper (Gt, e1, e2)),
                                  (InfixOper (Eq, e1, e2)))) env
    | InfixOper (Lte, e1, e2) ->
        lower_exp (InfixOper (Or, (InfixOper (Lt, e1, e2)),
                                  (InfixOper (Eq, e1, e2)))) env
    | InfixOper (op, e1, e2) ->
        let (r1, code1) = lower e1 in
        let (r2, code2) = lower e2 in
        let res = next () in
        res, code1 @ code2 @ [Binop (res, op, r1, r2)]
    | PrefixOper (Plus, e) -> lower e
    | PrefixOper (Minus, e) ->
        let (r, code) = lower e in
        let res = next () in
        res, code @ [Load (zr, IntLit 0); Binop (res, Minus, zr, r)]
    | PrefixOper (Not, e) ->
        let (r, code) = lower e in
        let res = next () in
        res, code @ [Unop (res, `Not, r)]
    | PrefixOper (_, _) -> failwith "unsupported prefix op in ast0"
    | Deref _ -> failwith "deref unsupported in ast0"
    | Ref _ -> failwith "ref unsupported in ast0"
    | SetEq (n, e) ->
        let (r, code) = lower e in
        let nr = Varenv.assoc n env in
        nr, code @ [Mov (nr, r)]
    | Funcall (Var "out", [arg]) ->
        let (cr, ccode) = lower arg in
        next (), ccode @ [ Output cr ]
    | Funcall (Var "in", [dst]) ->
        let ret = next () in
        ret, [ Input ret ]
    | Funcall (f, es) ->
        let (ers, ecodes) = List.split @@ List.map lower es in
        let pushes = List.map (fun x -> Push x) ers in
        let argument_eval = (List.concat ecodes) @ pushes in
        let ret = next () in
        (match f with
         | Var n ->
             ret, argument_eval @ [Call (Name n); Pop ret]
         | _ ->
             let (fr, fcode) = lower f in
             ret, fcode @ argument_eval @ [Call (Address fr); Pop ret]
        )

  let rec lower_stmt stmt env =
    match stmt with
    | Let (_, (n, _), e) ->
      let (r, code) = lower_exp e env in
      (Varenv.bind n r env), code
    | IfElse (cond, iftrue, iffalse) ->
        let (cr, ccode) = lower_exp cond env in
        let (env', tcode) = lower_block iftrue env in
        let (env', fcode) = lower_block iffalse env in
        env, ccode @ [If (cr, tcode, fcode)]
    | If (cond, iftrue) -> lower_stmt (IfElse (cond, iftrue, [])) env
    | Exp e -> let (_, ecode) = lower_exp e env in env, ecode
    | While (cond, body) ->
        let (cr, ccode) = lower_exp cond env in
        let (env', bcode) = lower_block body env in
        env, [While (cr, ccode, bcode)]
    | Return e ->
        (* TODO: jump to return address... *)
        let (r, code) = lower_exp e env in
        env, code @ [Push r]

  and lower_block block env =
    let f (env, code) stmt =
      let (env', code') = lower_stmt stmt env in
      env', code@code'
    in
    List.fold_left f (Varenv.newframe env, []) block

  let rec lower_topdef def env =
    match def with
    | Const ((n, _), e) ->
        let (r, code) = lower_exp e env in
        (Varenv.bind n r env), code
    | Fun (n, formals, _, body) ->
        let formalnames = List.map fst formals in
        let formalvars = List.map next formals in
        let env' = Varenv.bindlist formalnames formalvars env in
        let pops = List.map (fun x -> Pop x) formalvars in
        let (_, bodycode) = lower_block body env' in
        env, [Label n] @ pops @ bodycode

  let lower_prog (Prog defs) =
    let f (env, code) def =
      let (env', code') = lower_topdef def env in
      env', code@code'
    in
    List.fold_left f (Varenv.empty, []) (Ast.const_eval defs)
end
