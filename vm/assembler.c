#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

/*
  Instruction format is as follows:

  add r0 r1 r2
  out r1

  cmov r1 r2 r3

  I am reasonably sure that blank lines are OK, and that there must be
  precisely one space between instruction and register, and register and
  register. No extra spaces ANYWHERE ELSE. Unchecked runtime error.

  MAKE SURE YOU HAVE A BLANK LINE AT THE END!!!
*/

typedef uint32_t word;

struct prog {
        word *prog;
        word len;
};

void error (char *msg, char *optional_arg);
void check_usage (int argc);
FILE *open_file (char *fn, char *mode);
word file_lines (FILE *fp);
struct prog process_file (FILE *input);
void write_file (FILE *output, struct prog asm_);

int main (int argc, char **argv)
{
        check_usage(argc);

        FILE *input = open_file(argv[1], "r");
        struct prog asm_ = process_file(input);
        fclose(input);

        FILE *output = open_file(argv[2], "wb");
        write_file(output, asm_);
        fclose(output);

        free(asm_.prog);
}

void error (char *msg, char *optional_arg)
{
        fprintf(stderr, "Error: ");

        if (optional_arg) {
                fprintf(stderr, msg, optional_arg);
                putchar('\n');
        }
        else {
                fprintf(stderr, "%s\n", msg);
        }

        exit(EXIT_FAILURE);
}

void check_usage (int argc)
{
        if (argc != 3) {
                error("Invalid usage. Please run like: "
                      "./asm <input_filename> <output_filename>",
                      NULL);
        }
}

FILE *open_file (char *fn, char *mode)
{
        FILE *fp = fopen(fn, mode);

        if (fp == NULL) {
                error("Could not open file `%s'.", fn);
        }

        return fp;
}

word file_lines (FILE *fp)
{
        word lines = 0;

        /* Copied straight from http://stackoverflow.com/a/12733630/569183 */
        while (!feof(fp)) {
                if (fgetc(fp) == '\n') {
                        lines++;
                }
        }

        rewind(fp);
        return lines;
}

static int instr_lookup (char *instr)
{
        static char reverse_ops[][7] = { "cmov", "sload", "sstore", "add",
                                         "mult", "div", "nand", "halt", "map",
                                         "unmap", "out", "in", "loadp",
                                         "loadv"
                                       };
        static int NUM_INSTRS = 14;

        for (int i = 0; i < NUM_INSTRS; i++) {
                if (strcmp(instr, reverse_ops[i]) == 0) {
                        return i;
                }
        }

        return -1;
}

enum ops { CMOV = 0, SLOAD, SSTORE, ADD, MULT, DIV, NAND, HALT,
           MAP, UNMAP, OUT, IN, LOADP, LOADV
         };

static const char OP_WIDTH = 4;
static const char OP_LSB = 28; // 32 - OP_WIDTH;

static const unsigned char OP_MASK = 0xF;
static const unsigned char REG_MASK = 07;
static const unsigned VAL_MASK = 0x1FFFFFF;

static word three_register (unsigned char op, int ra, int rb, int rc)
{
        static const char RA_LSB = 6;
        static const char RB_LSB = 3;
        static const char RC_LSB = 0;

        word w = 0;
        w |= (op & OP_MASK) << OP_LSB;
        w |= (ra & REG_MASK) << RA_LSB;
        w |= (rb & REG_MASK) << RB_LSB;
        w |= (rc & REG_MASK) << RC_LSB;

        return w;
}

static word load_val (unsigned ra, unsigned val)
{
        static const char RA_LSB = 25;

        word w = 0;
        w |= (LOADV & OP_MASK) << OP_LSB;
        w |= (ra & REG_MASK) << RA_LSB;
        w |= (val & VAL_MASK);

        return w;
}

static unsigned read_reg (FILE *input)
{
        unsigned reg = 20;
        fscanf(input, " r%d", &reg);
        return reg;
}

static word read_val (FILE *input)
{
        word val = 0;
        fscanf(input, " %u", &val);
        return val;
}

static word read_instr (FILE *input, char *instr)
{
        int instr_num = instr_lookup(instr);
        word w = 0;

        switch (instr_num) {
                case -1:
                        error("Do not know instruction: `%s'.", instr);
                        break;

                case HALT: {
                                w = three_register(HALT, 0, 0, 0);
                                break;
                        }

                case MAP: {
                                unsigned char rb = read_reg(input);
                                unsigned char rc = read_reg(input);
                                w = three_register(MAP, 0, rb, rc);
                                break;
                        }

                case UNMAP: {
                                unsigned char rc = read_reg(input);
                                w = three_register(UNMAP, 0, 0, rc);
                                break;
                        }

                case OUT: {
                                unsigned char rc = read_reg(input);
                                w = three_register(OUT, 0, 0, rc);
                                break;
                        }

                case IN: {
                                unsigned char rc = read_reg(input);
                                w = three_register(IN, 0, 0, rc);
                                break;
                        }

                case LOADP: {
                                unsigned char rb = read_reg(input);
                                unsigned char rc = read_reg(input);
                                w = three_register(LOADP, 0, rb, rc);
                                break;
                        }

                case LOADV: {
                                unsigned char ra = read_reg(input);
                                word val = read_val(input);
                                w = load_val(ra, val);
                                break;
                        }

                default: {
                                unsigned char ra = read_reg(input);
                                unsigned char rb = read_reg(input);
                                unsigned char rc = read_reg(input);
                                w = three_register(instr_num, ra, rb, rc);
                                break;
                        }
        }

        return w;
}

struct prog process_file (FILE *input)
{
        struct prog asm_ = { NULL, file_lines(input) };

        word *assembled = calloc(asm_.len, sizeof * assembled);

        if (assembled == NULL) {
                error("Could not allocate memory for file contents.", NULL);
        }

        for (word i = 0; i < asm_.len; i++) {
                char instr[7];
                fscanf(input, "%s", instr);
                //printf("%d\tinstr: %s\n", i, instr);
                assembled[i] = read_instr(input, instr);
        }

        asm_.prog = assembled;

        return asm_;
}

void write_file (FILE *output, struct prog asm_)
{
        for (word i = 0; i < asm_.len; i++) {
                char *instr_chars = (void *) &asm_.prog[i];
                fwrite(&instr_chars[3], 1, 1, output);
                fwrite(&instr_chars[2], 1, 1, output);
                fwrite(&instr_chars[1], 1, 1, output);
                fwrite(&instr_chars[0], 1, 1, output);
        }
}
