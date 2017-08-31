type reg = [ `EAX | `EBX | `ECX | `EDX
           | `ESI | `EDI | `ESP | `EBP ]
type offset = int
type arg = [ `Reg of reg | `Imm of int ]
type adjusted = [ arg | `Offset of (offset * arg) ]
type space = [ adjusted | `Deref of adjusted ]

type op1 = space
type op2 = space * space
type op3 = space * space * space

let show_reg = function
    | `EAX -> "eax"
    | `EBX -> "ebx"
    | `ECX -> "ecx"
    | `EDX -> "edx"
    | `ESI -> "esi"
    | `EDI -> "edi"
    | `ESP -> "esp"
    | `EBP -> "ebp"

let rec show_op1 : space -> string = function
  | `Reg r -> show_reg r
  | `Imm i -> string_of_int i
  | `Offset (0, arg) -> show_op1 (Obj.magic arg)
  | `Offset (off, arg) ->
      let sgn = if off > 0 then "+" else "" in
      "[" ^ show_op1 (Obj.magic arg) ^ sgn ^ string_of_int off ^ "]"
  | `Deref arg -> "(" ^ show_op1 (Obj.magic arg) ^ ")"

let show_op2 (a, b) = show_op1 a ^ ", " ^ show_op1 b
let show_op3 (a, b, c) = show_op2 (a, b) ^ ", " ^ show_op1 c

type t = [
  | `Global of string
  | `Section of string
  | `Label of string
  | `Mov of op2
  | `Int of op1
]

let show = function
  | `Global s -> "global" ^ s
  | `Section s -> "section " ^ s
  | `Align i -> "align " ^ string_of_int i
  | `Label s -> s ^ ":"
  | `Mov o2 -> "mov\t" ^ show_op2 o2
  | `Int o1 -> "int\t" ^ show_op1 o1
