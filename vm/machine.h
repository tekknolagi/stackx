#ifndef MACHINE_H
#define MACHINE_H

#include <stdbool.h>

#include "word.h"
#include "memory.h"

enum ops { CMOV = 0, SLOAD, SSTORE, ADD, MULT, DIV, NAND, HALT,
           MAP, UNMAP, OUT, IN, LOADP, LOADV, NUM_OPS };

enum regs { R0 = 0, R1, R2, R3, R4, R5, R6, R7, NUM_REGS };

#define OP_WIDTH 4
#define OP_LSB (32 - OP_WIDTH)
#define REG_WIDTH 3
#define VAL_WIDTH (32 - OP_WIDTH - REG_WIDTH)
#define VAL_LSB 0

/* 4 bits */
#define INSTR_MASK 0xF

/* 3 bits */
#define REG_MASK 07U

/* 25 bits */
#define VAL_MASK 0x1FFFFFF

typedef struct machine *Machine_T;

#define T Machine_T

T machine_new ();
void machine_load (T machine, Seg_T seg0);
void machine_run (T machine);
void machine_free (T *machine);

#undef T
#endif
