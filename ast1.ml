module AST1 = struct
  type reg = R of int
  let string_of_reg (R i) = "r" ^ string_of_int i

  type op3 = reg * reg * reg
  let string_of_op3 (r1, r2, r3) =
    String.concat ", " (List.map string_of_reg [r1; r2; r3])

  type op2 = reg * reg
  let string_of_op2 (r1, r2) =
    String.concat ", " (List.map string_of_reg [r1; r2])

  type op1 = reg
  let string_of_op1 = string_of_reg

  type label = Ast0.AST0.label
  let string_of_label = Ast0.AST0.string_of_label

  type command =
    | LABEL of string
    | CJUMP of reg * label
    | JUMP of string
    | JUMPA of op1
    | SLOAD of op3
    | SSTORE of op3
    | INC of op1
    | DEC of op1
    | ADD of op3
    | SUB of op3
    | MULT of op3
    | DIV of op3
    | LT of op3
    | NAND of op3
    | HALT
    | MAP of op2
    | UNMAP of op1
    | OUTPUT of op1
    | INPUT of op1
    | LOADV of reg * int

  let string_of_command c =
    let s = function
    | CJUMP (r, l) -> "CJUMP " ^ string_of_reg r ^ " " ^ string_of_label l
    | JUMP s -> "JUMP " ^ s
    | JUMPA r -> "JUMP " ^ string_of_op1 r
    | SLOAD o -> "SLOAD " ^ string_of_op3 o
    | SSTORE o -> "SSTORE " ^ string_of_op3 o
    | INC r -> "INC " ^ string_of_op1 r
    | DEC r -> "DEC " ^ string_of_op1 r
    | ADD o -> "ADD " ^ string_of_op3 o
    | SUB o -> "SUB " ^ string_of_op3 o
    | MULT o -> "MULT " ^ string_of_op3 o
    | DIV o -> "DIV " ^ string_of_op3 o
    | LT o -> "LT " ^ string_of_op3 o
    | NAND o -> "NAND " ^ string_of_op3 o
    | HALT -> "HALT"
    | MAP o -> "MAP " ^ string_of_op2 o
    | UNMAP o -> "UNMAP " ^ string_of_op1 o
    | OUTPUT o -> "OUTPUT "  ^ string_of_op1 o
    | INPUT o -> "INPUT " ^ string_of_op1 o
    | LOADV (r, i) -> "LOADV " ^ string_of_reg r ^ ", $" ^ string_of_int i
    | _ -> failwith "can't happen"
    in
    match c with
    | LABEL l -> l ^ ":"
    | c' -> "  " ^ s c'

  let string_of_prog p = String.concat "\n" @@ List.map string_of_command p

  open Ast0.AST0
  let mklbl i = LABEL (string_of_label i)
  let stack = R 1
  let sp = R 2
  let spillblock = R 3
  let aux = R 4
  let tmp = R 5
  let dest = R 6
  let rec lower_prog p =
    let stacksize = 1000 in
    let spillsize = !Ast0.AST0.vnum in (* Number of variables *)
    [ LABEL "_start";
      LOADV (R 1, stacksize);
      MAP (stack, R 1);
      LOADV (R 3, spillsize);
      MAP (spillblock, R 3);
      LOADV (sp, 0);
      JUMP "main";
    ] @ lower_block p @ [UNMAP spillblock; UNMAP stack; HALT]
  and lower_block b = List.concat @@ List.map lower_command b
  and load dst (V i) = [ LOADV (aux, i); SLOAD (dst, spillblock, aux) ]
  and store r (V off) = [ LOADV (aux, off); SSTORE (spillblock, aux, r) ]
  and op3 vdst v1 v2 op = load (R 7) v1 @ load tmp v2 @ op @ store dest vdst
  and lower_command = function
    | Label s -> [LABEL s]
    | Load ((V dst), (IntLit i)) ->
        [LOADV (aux, i); LOADV (tmp, dst); SSTORE (spillblock, tmp, aux)]
    | Load (dst, (BoolLit true)) -> lower_command (Load (dst, IntLit 1))
    | Load (dst, (BoolLit false)) -> lower_command (Load (dst, IntLit 0))
    | Load (dst, (CharLit c)) -> lower_command (Load (dst, IntLit (Char.code c)))
    | Binop (dst, Ast.AST.Plus, v1, v2) ->
        op3 dst v1 v2 [ADD (dest, aux, tmp)]
    | Binop (dst, Ast.AST.Minus, v1, v2) ->
        op3 dst v1 v2 [SUB (dest, aux, tmp)]
    | Binop (dst, Ast.AST.Times, v1, v2) ->
        op3 dst v1 v2 [MULT (dest, aux, tmp)]
    | Binop (dst, Ast.AST.Div, v1, v2) ->
        op3 dst v1 v2 [DIV (dest, aux, tmp)]
    | Binop (dst, Ast.AST.Lt, v1, v2) ->
        op3 dst v1 v2 [LT (dest, aux, tmp)]
    | Binop (dst, Ast.AST.Gt, v1, v2) ->
        op3 dst v1 v2 [LT (dest, tmp, aux)]
    | Binop (_, _, _, _) -> failwith "unimplemented binary op in ast1"
    | Unop (dst, `Not, v1) ->
        load (R 7) v1 @ [NAND (dest, R 7, R 7)] @ store dest dst
    | Mov (dst, v1) ->
        load (R 7) v1 @ load tmp dst @ [SSTORE (spillblock, tmp, R 7)]
    | If (cv, iftrue, iffalse) ->
        let falseblock = lower_block iffalse in
        let truelbl = nextlbl () in
        let trueblock = lower_block iftrue in
        let endlbl = nextlbl () in
        load (R 7) cv @ [CJUMP (R 7, truelbl)]
        @ falseblock @ [JUMP (string_of_label endlbl)]
        @ (mklbl truelbl)::trueblock
        @ [mklbl endlbl]
    | Call (Name n) -> [JUMP n]
    | Call (Address a) -> load (R 7) a @ [JUMPA (R 7)]
    | Push v -> load (R 7) v @ [SSTORE (stack, sp, R 7); INC sp]
    | Pop dst ->
        load dest dst
        @ [DEC sp; SLOAD (R 7, stack, sp); SSTORE (spillblock, dest, R 7)]
    | While (cv, ccode, bcode) ->
        let condlbl = nextlbl () in
        let cblock = lower_block ccode in
        let bodylbl = nextlbl () in
        let bblock = lower_block bcode in
        let endlbl = nextlbl () in
        (mklbl condlbl)::cblock
        @ load (R 7) cv @ [CJUMP (R 7, bodylbl)]
        @ [JUMP (string_of_label endlbl); mklbl bodylbl]
        @ bblock
        @ [JUMP (string_of_label condlbl); mklbl endlbl]
    | Output v -> load (R 7) v @ [OUTPUT (R 7)]
    | Input dst -> [INPUT (R 7)] @ store (R 7) dst
end
