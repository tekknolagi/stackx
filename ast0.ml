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
  type command =
    | Load of reg * lit
    | Binop of reg * binop * reg * reg 
    | Unop of reg * unop * reg
    | Mov of reg * reg

  let set r = string_of_reg r ^ " <- "
  let string_of_command = function
    | Load (dst, l) ->
        set dst ^ string_of_lit l
    | Binop (dst, o, r1, r2)  ->
        set dst ^ string_of_reg r1 ^ Ast.AST.string_of_op o ^ string_of_reg r2
    | Unop (dst, o, r) ->
        set dst ^ string_of_unop o ^ string_of_reg r
    | Mov (dst, src) ->
        set dst ^ string_of_reg src

  let num = ref 0
  let next () = let i = !num in let () = num := !num + 1 in R i

  let rec lower_exp exp env =
    let open Ast.AST in
    let to_unop = function
      | Not -> `Not
      | Plus -> `Plus
      | Minus -> `Minus
      | _ -> failwith "unsupported unary operator"
    in
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
        let (r1, code1) = lower_exp e1 env in
        let (r2, code2) = lower_exp e2 env in
        let res = next () in
        res, code1 @ code2 @ [Binop (res, op, r1, r2)]
    | PrefixOper (op, e) ->
        let (r, code) = lower_exp e env in
        let res = next () in
        res, code @ [Unop (res, to_unop op, r)]
    | Ref e ->
        let (r, code) = lower_exp e env in
        let res = next () in
        res, code @ [Unop (res, `Ref, r)]
    | Deref e ->
        let (r, code) = lower_exp e env in
        let res = next () in
        res, code @ [Unop (res, `Deref, r)]
    | SetEq (n, e) ->
        let (r, code) = lower_exp e env in
        let nr = Varenv.assoc n env in
        nr, code @ [Mov (nr, r)]
    | _ -> failwith "unsupported"
end
