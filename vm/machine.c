#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "machine.h"

struct machine {
        word regs[NUM_REGS];
        Mem_T mem;
};

Machine_T machine_new ()
{
	Machine_T m = calloc(1, sizeof *m);
	assert(m != NULL);

        /* Set all the registers to 0. */
        memset(m->regs, 0, NUM_REGS * sizeof(word));

        return m;
}

void machine_load (Machine_T m, Seg_T seg0)
{
        assert(m != NULL);
        assert(seg0 != NULL);

        m->mem = mem_new(seg0);
}

#ifdef HISTOGRAM
struct instr {
	int num;
	size_t uses;
};

int cmpfunc (const void *a, const void *b)
{
	struct instr *a_i = a, *b_i = b;
	return a_i->uses - b_i->uses;
}
#endif

void machine_run (Machine_T m)
{
        assert(m != NULL);

#ifdef HISTOGRAM
	struct instr uses[NUM_OPS];
	for (int i = 0; i < NUM_OPS; i++) {
		uses[i].num = i;
		uses[i].uses = 0;
	}
#endif

	Mem_T mem = m->mem;
	word *regs = m->regs;
	Seg_T seg0_seg = mem_seg(m->mem, 0);
	word *seg0 = seg_get_contents(seg0_seg);

	/* Instruction pointer. */
	word ip = 0;

        while (true) {
		word current_word = seg0[ip++];
		unsigned char instr = (current_word >> 28) & INSTR_MASK;

		word ra = (current_word >> 6) & REG_MASK;
		word rb = (current_word >> 3) & REG_MASK;
		word rc = current_word & REG_MASK;

#ifdef HISTOGRAM
		uses[instr].uses++;
#endif

		/* Operations are ordered by % usage in sandmark.umz. Compile
		 * with -DHISTOGRAM to verify. */
		if (instr == LOADV) {
			word ra = (current_word >> 25) & REG_MASK;
			word val = current_word & VAL_MASK;
			regs[ra] = val;
		}
		else if (instr == SLOAD) {
			regs[ra] = mem_load(mem, regs[rb], regs[rc]);
		}
		else if (instr == SSTORE) {
			mem_store(mem, regs[ra], regs[rb], regs[rc]);
		}
		else if (instr == NAND) {
			regs[ra] = ~(regs[rb] & regs[rc]);
		}
		else if (instr == LOADP) {
			word rb_val = regs[rb];
			if (rb_val != 0) {
				mem_dup(mem, rb_val);
				seg0_seg = mem_seg(mem, 0);
				seg0 = seg_get_contents(seg0_seg);
			}

			ip = regs[rc];
		}
		else if (instr == ADD) {
			regs[ra] = regs[rb] + regs[rc];
		}
		else if (instr == CMOV) {
			if (regs[rc] != 0) {
				regs[ra] = regs[rb];
			}
		}
		else if (instr == MAP) {
			regs[rb] = mem_map(mem, regs[rc]);
		}
		else if (instr == UNMAP) {
			mem_unmap(mem, regs[rc]);
		}
		else if (instr == DIV) {
			regs[ra] = regs[rb] / regs[rc];
		}
		else if (instr == MULT) {
			regs[ra] = regs[rb] * regs[rc];
		}
		else if (instr == OUT) {
			/* Only lowest 8 bits allowed. */
			putc(regs[rc] & 0xFF, stdout);
		}
		else if (instr == HALT) {
			break;
		}
		else if (instr == IN) {
			int c = getc(stdin);

			if (c == EOF) {
				/* All 1s. */
				regs[rc] = ~(0U);
			}
			else {
				/* Only lowest 8 bits allowed. */
				regs[rc] = c & 0xFF;
			}
		}
		else {
			;
		}
        }

#ifdef HISTOGRAM
	qsort(uses, NUM_OPS, sizeof(struct instr), cmpfunc);

	for (int i = 0; i < NUM_OPS; i++) {
		fprintf(stderr, "%s\t%lu\n",
			reverse_ops[uses[i].num],
			uses[i].uses);
	}
#endif
}

void machine_free (Machine_T *m)
{
        assert(m != NULL);
        assert(*m != NULL);

        mem_free(&(*m)->mem);
        free(*m);
        *m = NULL;
}

