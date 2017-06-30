package main

type Op int

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
