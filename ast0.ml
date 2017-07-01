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
    | Call of callable
    | While of reg * command list * command list
    | Push of reg (* Push a register onto the global stack. *)
    | Pop of reg  (* Pop a value from the global stack into a register. *)
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
    | Call (f) ->
        "call " ^ string_of_callable f
    | While (cr, cond, body) ->
        "while (" ^ string_of_reg cr ^ ") {\n" ^ String.concat "\n" (List.map
        string_of_command cond) ^ "\n} | {\n" ^ String.concat "\n" (List.map
        string_of_command body) ^ "\n}"
    | Push r -> "push " ^ string_of_reg r
    | Pop r -> "pop " ^ string_of_reg r
    | Label n -> n ^ ":"

  let string_of_program p =
    String.concat "\n" (List.map string_of_command p)

  let num = ref 0
  let next _ = let i = !num in let () = num := !num + 1 in R i

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
        let formalregs = List.map next formals in
        let env' = Varenv.bindlist formalnames formalregs env in
        let pops = List.map (fun x -> Pop x) formalregs in
        let (_, bodycode) = lower_block body env' in
        env, [Label n] @ pops @ bodycode

  let lower_prog (Prog defs) =
    let f (env, code) def =
      let (env', code') = lower_topdef def env in
      env', code@code'
    in
    List.fold_left f (Varenv.empty, []) defs
end
