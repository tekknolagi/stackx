let zero  = `Reg 0x0
let ip    = `Reg 0xA
let sp    = `Reg 0xB
let ret   = `Reg 0xC
let tmp   = `Reg 0xD
let flags = `Reg 0xE

type reg = int
type arg = [ `Reg of reg | `Imm of int ]
type space = [ arg | `Deref of arg ]
type op1 = space
type op2 = space * space
type op3 = space * space * space

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
