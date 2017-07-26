let zero  = `Arg (`Reg 0x0)
let ip    = `Arg (`Reg 0xA)
let sp    = `Arg (`Reg 0xB)
let ret   = `Arg (`Reg 0xC)
let tmp   = `Arg (`Reg 0xD)
let flags = `Arg (`Reg 0xE)

type reg = int
type arg = [ `Reg of reg | `Imm of int ]
type space = [ `Arg of arg | `Deref of arg ]
type op1 = space
type op2 = space * space
type op3 = space * space * space
