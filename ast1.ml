module AST1 = struct
  type reg = Ast0.AST0.reg
  let string_of_reg = Ast0.AST0.string_of_reg

  type op3 = reg * reg * reg
  let string_of_op3 (r1, r2, r3) =
    String.concat ", " (List.map Ast0.AST0.string_of_reg [r1; r2; r3])

  type op2 = reg * reg
  let string_of_op2 (r1, r2) =
    String.concat ", " (List.map Ast0.AST0.string_of_reg [r1; r2])

  type op1 = reg
  let string_of_op1 = Ast0.AST0.string_of_reg

  type label = L of int
  let string_of_label (L i) = "_L" ^ string_of_int i

  let num = ref 0
  let nextlbl _ = let i = !num in let () = num := !num + 1 in L i

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
    | NAND of op3
    | MOV of op2
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
    | NAND o -> "NAND " ^ string_of_op3 o
    | MOV o -> "MOV " ^ string_of_op2 o
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
  let rec lower_prog p =
    let stacksize = 1000 in
    [ LABEL "_start";
      LOADV (R 1, stacksize);
      MAP (stack, R 1);
      LOADV (sp, 0);
      JUMP "main";
  ] @ List.concat @@ List.map lower_command p
  and lower_command = function
    | Label s -> [LABEL s]
    | Load (dst, (IntLit i)) -> [LOADV (dst, i)]
    | Load (dst, (BoolLit true)) -> [LOADV (dst, 1)]
    | Load (dst, (BoolLit false)) -> [LOADV (dst, 0)]
    | Load (dst, (CharLit c)) -> [LOADV (dst, Char.code c)]
    | Binop (dst, Ast.AST.Plus, r1, r2) -> [ADD (dst, r1, r2)]
    | Binop (dst, Ast.AST.Minus, r1, r2) -> [SUB (dst, r1, r2)]
    | Binop (dst, Ast.AST.Times, r1, r2) -> [MULT (dst, r1, r2)]
    | Binop (dst, Ast.AST.Div, r1, r2) -> [DIV (dst, r1, r2)]
    | Binop (_, _, _, _) -> failwith "unimplemented binary op in ast1"
    | Unop (dst, `Not, r1) -> [NAND (dst, r1, r1)]
    | Mov (dst, r1) -> [MOV (dst, r1)]
    | If (cr, iftrue, iffalse) ->
        let falseblock = lower_prog iffalse in
        let truelbl = nextlbl () in
        let trueblock = lower_prog iftrue in
        let endlbl = nextlbl () in
        [CJUMP (cr, truelbl)]
        @ falseblock @ [JUMP (string_of_label endlbl)]
        @ (mklbl truelbl)::trueblock
    | Call (Name n) -> [JUMP n]
    | Call (Address a) -> [JUMPA a]
    | Push r -> [SSTORE (stack, sp, r); INC sp]
    | Pop r -> [DEC sp; SLOAD (stack, sp, r)]
end
