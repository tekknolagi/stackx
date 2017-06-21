#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

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

Seg_T fileio_read (char *fn);

int main (int argc, char **argv) {
    (void)argc;

    char *fn = argv[1];
    FILE *fp = fopen(fn, "rb");
    assert(fp != NULL);

    struct stat fp_info;
    int success = stat(fn, &fp_info);
    assert(success == 0);

    Seg_T prog = seg_new(fp_info.st_size / sizeof(word), 0);
    word ind = 0;

    while (feof(fp) == 0) {
        /* Four byte array. */
        unsigned char current_word_chars[4];

        /* Read the bytes in backwards. Because endian-ness. */
        int nread = 0;
        nread += fread(&current_word_chars[3], 1, 1, fp);
        nread += fread(&current_word_chars[2], 1, 1, fp);
        nread += fread(&current_word_chars[1], 1, 1, fp);
        nread += fread(&current_word_chars[0], 1, 1, fp);
        assert(nread == 4);

        /* Since arrays are contiguous, we can interpret the
           four-byte char array as one 32-bit word. */
        word current_word = *(word *) current_word_chars;
        prog->contents[ind++] = current_word;
    }

    fclose(fp);

    word regs[NUM_REGS] = { 0 };
    Mem_T mem = mem_new(prog);

    Seg_T seg0_seg = Seq_get(mem->segs, 0);
    word *seg0 = seg0_seg->contents;

    /* Instruction pointer. */
    word ip = 0;

    bool running = true;
    while (running) {
        word current_word = seg0[ip++];
        unsigned char instr = (current_word >> 28) & INSTR_MASK;

        word ra = (current_word >> 6) & REG_MASK;
        word rb = (current_word >> 3) & REG_MASK;
        word rc = current_word & REG_MASK;

        switch (instr) {
            case LOADV: {
                word ra = (current_word >> 25) & REG_MASK;
                word val = current_word & VAL_MASK;
                regs[ra] = val;
                break;
            }
            case SLOAD:
                regs[ra] = mem_load(mem, regs[rb], regs[rc]);
                break;
            case SSTORE:
                mem_store(mem, regs[ra], regs[rb], regs[rc]);
                break;
            case NAND:
                regs[ra] = ~(regs[rb] & regs[rc]);
                break;
            case LOADP: {
                word rb_val = regs[rb];
                if (rb_val != 0) {
                    /* Duplicate the segment. */
                    word segid = rb_val;
                    Seg_T seg = Seq_get(mem->segs, segid);
                    Seg_T duplicated = seg_new(seg->len, 0);
                    memcpy(duplicated->contents, seg->contents, seg->len * sizeof(word));

                    /* Free segment 0. */
                    free(Seq_get(mem->segs, 0));

                    /* Put the duplicate in segment 0. */
                    Seq_put(mem->segs, 0, duplicated);

                    seg0 = duplicated->contents;
                }

                ip = regs[rc];
                break;
            }
            case ADD:
                regs[ra] = regs[rb] + regs[rc];
                break;
            case CMOV:
                if (regs[rc] != 0) {
                    regs[ra] = regs[rb];
                }
                break;
            case MAP:
                regs[rb] = mem_map(mem, regs[rc]);
                break;
            case UNMAP:
                mem_unmap(mem, regs[rc]);
                break;
            case DIV:
                regs[ra] = regs[rb] / regs[rc];
                break;
            case MULT:
                regs[ra] = regs[rb] * regs[rc];
                break;
            case OUT:
                /* Only lowest 8 bits allowed. */
                putc(regs[rc] & 0xFF, stdout);
                break;
            case HALT:
                running = false;
                break;
            case IN: {
                int c = getc(stdin);

                if (c == EOF) {
                    /* All 1s. */
                    regs[rc] = ~(0U);
                }
                else {
                    /* Only lowest 8 bits allowed. */
                    regs[rc] = c & 0xFF;
                }
                break;
            }
        }
    }

    mem_free(&mem);

    return 0;
}
