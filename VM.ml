let zero  = `Reg 0x0
let ip    = `Reg 0xA
let sp    = `Reg 0xB
let ret   = `Reg 0xC
let tmp   = `Reg 0xD
let flags = `Reg 0xE

type reg = int
type offset = int
type arg = [ `Reg of reg | `Imm of int ]
type space = [ arg | `Deref of (offset * arg) ]
type op1 = space
type op2 = space * space
type op3 = space * space * space

let rec show_op1 : space -> string = function
  | `Reg 0x0 -> "zero"
  | `Reg 0xA -> "ip"
  | `Reg 0xB -> "sp"
  | `Reg 0xC -> "ret"
  | `Reg 0xD -> "tmp"
  | `Reg 0xE -> "flags"
  | `Reg r -> "r" ^ string_of_int r
  | `Imm i -> "$" ^ string_of_int i
  | `Deref (0, arg) -> "(" ^ show_op1 (Obj.magic arg) ^ ")"
  | `Deref (off, arg) ->
      string_of_int off ^ "(" ^ show_op1 (Obj.magic arg) ^ ")"

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
