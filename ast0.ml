module AST0 = struct
  type lit = IntLit of int | CharLit of char
  let string_of_lit = function
    | IntLit i -> string_of_int i ^ "i"
    | CharLit c -> "'" ^ Char.escaped c ^ "'"
  type binop = Ast.AST.op
  type unop = [ `Not | `Plus | `Minus | `Ref | `Deref ]
  let string_of_unop = function
    | `Not -> "not"
    | `Plus -> "+"
    | `Minus -> "-"
    | `Ref -> "&"
    | `Deref -> "*"
  type reg = R of int
  let string_of_reg (R i) = "r" ^ string_of_int i
  type callable = Name of string | Address of reg
  let string_of_callable = function
    | Name n -> n
    | Address r -> "(" ^ string_of_reg r ^ ")"
  type command =
    | Load of reg * lit
    | Binop of reg * binop * reg * reg
    | Unop of reg * unop * reg
    | Mov of reg * reg
    | If of reg * command list * command list
    | Call of callable * reg list
    | Label of string

  let set r = string_of_reg r ^ " <- "
  let rec string_of_command = function
    | Load (dst, l) ->
        set dst ^ string_of_lit l
    | Binop (dst, o, r1, r2)  ->
        set dst ^ string_of_reg r1 ^ Ast.AST.string_of_op o ^ string_of_reg r2
    | Unop (dst, o, r) ->
        set dst ^ string_of_unop o ^ string_of_reg r
    | Mov (dst, src) ->
        set dst ^ string_of_reg src
    | If (cond, iftrue, iffalse) ->
        "if (" ^ string_of_reg cond ^ ") {\n" ^ String.concat "\n" (List.map
        string_of_command iftrue) ^ "\n} else {\n" ^ String.concat "\n" (List.map
        string_of_command iffalse) ^ "\n}"
    | Call (f, args) ->
        "call " ^ string_of_callable f ^ "(" ^ String.concat "," (List.map
        string_of_reg args) ^ ")"
    | Label n -> n ^ ":"

  let string_of_program p =
    String.concat "\n" (List.map string_of_command p)

  let num = ref 0
  let next () = let i = !num in let () = num := !num + 1 in R i

  open Ast.AST

  let rec lower_exp exp env =
    let to_unop = function
      | Not -> `Not
      | Plus -> `Plus
      | Minus -> `Minus
      | _ -> failwith "unsupported unary operator"
    in
    let lower e = lower_exp e env in
    match exp with
    | IntLit i ->
        let r = next () in
        r, [ Load (r, IntLit i) ]
    | CharLit c ->
        let r = next () in
        r, [ Load (r, CharLit c) ]
    | Var n ->
        Varenv.assoc n env, []
    | InfixOper (op, e1, e2) ->
        let (r1, code1) = lower e1 in
        let (r2, code2) = lower e2 in
        let res = next () in
        res, code1 @ code2 @ [Binop (res, op, r1, r2)]
    | PrefixOper (op, e) ->
        let (r, code) = lower e in
        let res = next () in
        res, code @ [Unop (res, to_unop op, r)]
    | Ref e ->
        let (r, code) = lower e in
        let res = next () in
        res, code @ [Unop (res, `Ref, r)]
    | Deref e ->
        let (r, code) = lower e in
        let res = next () in
        res, code @ [Unop (res, `Deref, r)]
    | SetEq (n, e) ->
        let (r, code) = lower e in
        let nr = Varenv.assoc n env in
        nr, code @ [Mov (nr, r)]
    | Funcall (Var n, es) ->
        let rs_codes = List.map lower es in
        let (ers, ecodes) = (List.map fst rs_codes, List.map snd rs_codes) in
        next (), (List.concat ecodes) @ [Call (Name n, ers)]
    | Funcall (f, es) ->
        let (fr, fcode) = lower f in
        let rs_codes = List.map lower es in
        let (ers, ecodes) = (List.map fst rs_codes, List.map snd rs_codes) in
        next (), fcode @ (List.concat ecodes) @ [Call (Address fr, ers)]

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
    | _ -> failwith "unimplemented"

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
        let (_, bodycode) = lower_block body env in
        (* TODO: This is wrong. Fix it. *)
        (* Doesn't handle parameters at all. *)
        env, (Label n)::bodycode

  let lower_prog (Prog defs) =
    let f (env, code) def =
      let (env', code') = lower_topdef def env in
      env', code@code'
    in
    List.fold_left f (Varenv.empty, []) defs
end
