package main

type Op int

const (
	CJUMP Op = iota
	SLOAD
	SSTORE
	ADD
	SUB
	MULT
	DIV
	LT
	NAND
	HALT
	MAP
	UNMAP
	OUTPUT
	INPUT
	LOADV
)
