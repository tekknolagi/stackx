package main

import (
	"bytes"
	"encoding/binary"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
)

type Op int

//go:generate stringer -type=Op
const (
	CMOV Op = iota
	SLOAD
	SSTORE
	ADD
	MULT
	DIV
	NAND
	HALT
	MAP
	UNMAP
	OUTPUT
	INPUT
	LOADP
	LOADV
)

func run(program []uint32) {
	reg := [8]uint32{0, 0, 0, 0, 0, 0, 0, 0}
	platters := [][]uint32{program}
	freePlatters := []uint32{}
	var pc uint32

	for {
		instruction := platters[0][pc]
		op := Op((instruction >> 28) & 15)
		a := ((instruction >> 6) & 7)
		b := ((instruction >> 3) & 7)
		c := ((instruction >> 0) & 7)
		switch op {
		case CMOV:
			if reg[c] != 0 {
				reg[a] = reg[b]
			}
		case SLOAD:
			reg[a] = platters[reg[b]][reg[c]]
		case SSTORE:
			platters[reg[a]][reg[b]] = reg[c]
		case ADD:
			reg[a] = reg[b] + reg[c]
		case MULT:
			reg[a] = reg[b] * reg[c]
		case DIV:
			reg[a] = reg[b] / reg[c]
		case NAND:
			reg[a] = ^(reg[b] & reg[c])
		case HALT:
			return
		case MAP:
			{
				newPlatter := make([]uint32, reg[c])
				if len(freePlatters) > 0 {
					platters[freePlatters[0]] = newPlatter
					reg[b] = freePlatters[0]
					freePlatters = freePlatters[1:]
				} else {
					platters = append(platters, newPlatter)
					reg[b] = uint32(len(platters) - 1)
				}
			}
		case UNMAP:
			{
				platters[reg[c]] = nil
				freePlatters = append(freePlatters, reg[c])
			}
		case OUTPUT:
			os.Stdout.Write([]byte{byte(reg[c])})
		case INPUT:
			{
				b := []byte{0}
				_, err := os.Stdin.Read(b)
				check(err)
				reg[c] = uint32(b[0])
			}
		case LOADP:
			{
				if reg[b] != 0 {
					platters[0] = make([]uint32, len(platters[reg[b]]))
					copy(platters[0], platters[reg[b]])
				}
				pc = reg[c]
				continue
			}
		case LOADV:
			reg[(instruction>>25)&7] = instruction & 0x01FFFFFF
		default:
			panic(fmt.Errorf("Failed on %d", op))
		}
		pc++
	}
}

func readPlatters(path string) []uint32 {
	b, err := ioutil.ReadFile(path)
	check(err)
	platters := make([]uint32, len(b)/4)
	err = binary.Read(bytes.NewReader(b), binary.BigEndian, &platters)
	check(err)
	return platters
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func disassemble(program []uint32) {
	for i := range program {
		instruction := program[i]
		op := Op((instruction >> 28) & 15)
		a := ((instruction >> 6) & 7)
		b := ((instruction >> 3) & 7)
		c := ((instruction >> 0) & 7)
		var text string
		switch op {
		case CMOV:
			fallthrough
		case SLOAD:
			fallthrough
		case SSTORE:
			fallthrough
		case ADD:
			fallthrough
		case MULT:
			fallthrough
		case DIV:
			fallthrough
		case NAND:
			text = fmt.Sprintf("%s %%%d, %%%d, %%%d", op.String(), a, b, c)
		case HALT:
			text = fmt.Sprintf("%s", op.String())
		case MAP:
			fallthrough
		case LOADP:
			text = fmt.Sprintf("%s %%%d, %%%d", op.String(), b, c)
		case LOADV:
			text = fmt.Sprintf("%s %%%d, %x", op.String(), (instruction>>25)&7, instruction&0x01FFFFFFc)
		case UNMAP:
			fallthrough
		case OUTPUT:
			fallthrough
		case INPUT:
			text = fmt.Sprintf("%s %%%d", op.String(), c)
		}
		fmt.Printf("[%08x] %08x: %s\n", i, instruction, text)
	}
}

func rtl(program []uint32) {
	for i := range program {
		instruction := program[i]
		op := Op((instruction >> 28) & 15)
		a := ((instruction >> 6) & 7)
		b := ((instruction >> 3) & 7)
		c := ((instruction >> 0) & 7)
		var text string
		switch op {
		case CMOV:
			text = fmt.Sprintf("IF REG[%d] != 0 { REG[%d] = REG[%d] }", c, a, b)
		case SLOAD:
			text = fmt.Sprintf("REG[%d] = PLATTERS[REG[%d]][REG[%d]]", a, b, c)
		case SSTORE:
			text = fmt.Sprintf("PLATTERS[REG[%d]][REG[%d]] = REG[%d]", a, b, c)
		case ADD:
			text = fmt.Sprintf("REG[%d] = REG[%d] + REG[%d]", a, b, c)
		case MULT:
			text = fmt.Sprintf("REG[%d] = REG[%d] * REG[%d]", a, b, c)
		case DIV:
			text = fmt.Sprintf("REG[%d] = REG[%d] / REG[%d]", a, b, c)
		case NAND:
			text = fmt.Sprintf("REG[%d] = ^(REG[%d] & REG[%d])", a, b, c)
		case HALT:
			text = fmt.Sprintf("HALT")
		case MAP:
			text = fmt.Sprintf("REG[%d] = MALLOC(REG[%d])", b, c)
		case UNMAP:
			text = fmt.Sprintf("FREE(REG[%d])", c)
		case OUTPUT:
			text = fmt.Sprintf("OUTPUT(REG[%d])", c)
		case INPUT:
			text = fmt.Sprintf("REG[%d] = INPUT()", c)
		case LOADP:
			text = fmt.Sprintf("PLATTERS[0] = PLATTERS[REG[%d]]; GOTO REG[%d]", b, c)
		case LOADV:
			text = fmt.Sprintf("REG[%d] = %x", (instruction>>25)&7, instruction&0x01FFFFFFc)
		}
		fmt.Printf("[%08x] %08x: %s\n", i, instruction, text)
	}
}

func main() {
	printDisassembly := flag.Bool("disassemble", false, "Print instructions instead of running.")
	printRTL := flag.Bool("rtl", false, "Print RTL semantics for instructions instead of running or disassembling.")
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Println("Usage:")
		fmt.Println("  um [file]")
		fmt.Println("  um -disassemble [file]")
		fmt.Println("  um -rtl [file]")
		return
	}

	platters := readPlatters(flag.Arg(0))
	if *printDisassembly {
		disassemble(platters)
	} else if *printRTL {
		rtl(platters)
	} else {
		run(platters)
	}
}
