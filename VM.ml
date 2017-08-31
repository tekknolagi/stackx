type reg = EAX | EBX | ECX | EDX | EDI | ESI | EBP | ESP | EIP
let ret = `Reg EAX
let eax = ret
let sp = `Reg ESP
let bp = `Reg EBP
type offset = int
type arg = [ `Reg of reg | `Imm of int | `Label of string ]
type adjusted = [ arg | `Offset of (offset * arg) ]
type space = [ adjusted | `Deref of adjusted ]
type op1 = space
type op2 = space * space
type op3 = space * space * space

let show_reg = function
  | EAX -> "eax" | EBX -> "ebx" | ECX -> "ecx" | EDX -> "edx"
  | EDI -> "edi" | ESI -> "esi" | EBP -> "ebp" | ESP -> "esp" | EIP -> "eip"

let rec show_op1 : space -> string = function
  | `Label s -> s
  | `Reg r -> show_reg r
  | `Imm i -> "$" ^ string_of_int i
  | `Offset (0, arg) -> show_op1 (Obj.magic arg)
  | `Offset (off, arg) ->
      let sgn = if off > 0 then "+" else "" in
      "[" ^ show_op1 (Obj.magic arg) ^ sgn ^ string_of_int (off*4) ^ "]"
  | `Deref arg -> "(" ^ show_op1 (Obj.magic arg) ^ ")"

let show_op2 (a, b) = show_op1 a ^ ", " ^ show_op1 b
let show_op3 (a, b, c) = show_op2 (a, b) ^ ", " ^ show_op1 c

module Counter = struct
  type t = { mutable counter : int; base : string }
  let make s = { counter = 0; base = s }
  let inc ctr =
    let c = ctr.counter in
    ctr.counter <- ctr.counter + 1;
    c
  let next ctr =
    let c = inc ctr in
    ctr.base ^ string_of_int c
end
