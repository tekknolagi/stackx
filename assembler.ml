let rec find_labels offset instrs =
  let open Ast1.AST1 in
  match instrs with
  | [] -> []
  | (LABEL l)::rest -> (l, offset)::(find_labels offset rest)
  | _::rest -> find_labels (offset+1) rest

type reg = Ast1.AST1.reg
type op3 = Ast1.AST1.op3
type op2 = Ast1.AST1.op2
type op1 = Ast1.AST1.op1

type command =
  | UCJUMP of op2
  | USLOAD of op3
  | USSTORE of op3
  | UADD of op3
  | USUB of op3
  | UMULT of op3
  | UDIV of op3
  | ULT of op3
  | UNAND of op3
  | UHALT
  | UMAP of op2
  | UUNMAP of op1
  | UOUTPUT of op1
  | UINPUT of op1
  | ULOADV of reg * int

let lower_prog label_env prog =
  let getlabel l = List.assoc l label_env in
  let open Ast1.AST1 in
  let lower_command = function
    | CJUMP (c, l) ->
        [ULOADV (tmp, getlabel @@ string_of_label l); UCJUMP (c, tmp)]
    | JUMP l ->
        [ULOADV (tmp, getlabel l); ULOADV (R 7, 1); UCJUMP (R 7, tmp)]
    | JUMPA a -> [ULOADV (R 7, 1); UCJUMP (R 7, a)]
    | SLOAD o3 -> [USLOAD o3]
    | SSTORE o3 -> [USSTORE o3]
    | INC r -> [ULOADV (tmp, 1); UADD (r, r, tmp)]
    | DEC r -> [ULOADV (tmp, 1); USUB (r, r, tmp)]
    | ADD o3 -> [UADD o3]
    | SUB o3 -> [USUB o3]
    | MULT o3 -> [UMULT o3]
    | DIV o3 -> [UDIV o3]
    | LT o3 -> [ULT o3]
    | NAND o3 -> [UNAND o3]
    | HALT -> [UHALT]
    | MAP o2 -> [UMAP o2]
    | UNMAP o1 -> [UUNMAP o1]
    | OUTPUT o1 -> [UOUTPUT o1]
    | INPUT o1 -> [UINPUT o1]
    | LOADV (r, i) -> [ULOADV (r, i)]
    | LABEL _ -> failwith "labels shouldn't exist at this stage"
  in
  List.concat (List.map lower_command prog)

let assemble prog =
  let label_env = find_labels 0 prog in
  let not_label = function | Ast1.AST1.LABEL _ -> false | _ -> true in
  let without_labels = List.filter not_label prog in
  lower_prog label_env without_labels
