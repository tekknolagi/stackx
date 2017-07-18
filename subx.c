// SubX: VM modeling a subset of the 32-bit x86 ISA

#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "test.h"

#include "function_list"  // auto-generated function prototypes

// poor man's static_cast
#define CAST(x, y)  ((x)(y))

//// x86 data structures: registers and memory

enum {
  EAX,
  ECX,
  EDX,
  EBX,
  ESP,
  EBP,
  ESI,
  EDI,
  NUM_INT_REGISTERS,
};

enum {
  XMM0,
  XMM1,
  XMM2,
  XMM3,
  XMM4,
  XMM5,
  XMM6,
  XMM7,
  NUM_FLOAT_REGISTERS,
};

typedef union {
  int32_t i;
  uint32_t u;
} reg;

reg r[NUM_INT_REGISTERS] = { {0} };
float xmm[NUM_FLOAT_REGISTERS] = {0};
unsigned int EIP = 0;
// subset of flags
bool OF=false, ZF=false, SF=false;

uint8_t* mem = NULL;  // simulated memory
uint32_t mem_size = 0;

//// core VM

int main() {
  atexit(reset);
  // run on a 32-bit system
  assert(sizeof(int) == 4);
  assert(sizeof(float) == 4);
  // just always run tests for now
  run_tests();
  return 0;
}

// clean up at the start of each test, and before exit
void reset(void) {
  bzero(r, sizeof(r));
  bzero(xmm, sizeof(xmm));
  EIP = 0;
  OF = ZF = SF = false;
  if (mem != NULL) {
    free(mem);
    mem = NULL;
    mem_size = 0;
  }
}

// run_tests() runs all functions starting with 'test_'.
// all tests should have this structure:
//   function prototype: void test_...(void)
//   load_program("some string of bytes in hex separated by spaces"
//                "one instructions to a line; c will concatenate string literals");
//   run();
//   post checks
void test_add_imm32_to_eax(void) {
  load_program(
    // opcodes    modrm     sib       displacement      immediate
    "05                                                 0a 0b 0c 0d"  // add EAX 0x0d0c0b0a
  );
  run();
  CHECK(r[EAX].u == 0x0d0c0b0a);
}

void load_program(char* prog) {
  mem_size = word_count(prog);
  assert(mem == NULL);
  mem = calloc(mem_size, sizeof(*mem));  // https://stackoverflow.com/questions/19785518/is-dereferencing-null-pointer-valid-in-sizeof-operation
  assert(mem != NULL);
  char* curr = prog;
  char* end = prog + strlen(prog);
  for (size_t i = 0;  i < mem_size;  ++i) {
    assert (curr < end);
    mem[i] = strtol(curr, &curr, /*hex*/16);
//?     printf("%hhx\n", mem[i]);
  }
}

uint32_t word_count(const char* s) {
  uint32_t result = 0;
  bool at_space = true;
  for (;  *s != '\0';  ++s) {
    bool curr_is_space = isspace(*s);
    if (at_space && !curr_is_space) ++result;
    at_space = curr_is_space;
  }
//?   printf("word count: %u\n", result);
  return result;
}

void run(void) {
  EIP = 0;
  while (EIP < mem_size) {
    run_one_instruction();
  }
}

void run_one_instruction() {
  switch (next()) {
    case 0x05: {  // add EAX, imm32
      int32_t arg2 = imm32();
      int64_t tmp = r[EAX].i + arg2;
      r[EAX].i += arg2;
      SF = (r[EAX].i < 0);
      ZF = (r[EAX].i == 0);
      OF = (r[EAX].i != tmp);
      break;
    }
    case 0x81: {  // add r/m32, imm32
      // test case: add %eax, 0xd => 01 05 00 00 00
      uint8_t modrm = next();
      uint8_t mod = (modrm>>6);
      // ignore middle 3 'reg opcode' bits
      uint8_t rm = modrm & 0x7;
      int32_t* effective_address = 0;
      // Table 2-2 in the Intel manual, Volume 2
      switch (mod) {
        case 0:
          switch (rm) {
          default:
            // mod 0 is usually register addressing
            effective_address = CAST(int*, &mem[r[rm].u]);  // rely on the host itself being in little-endian order
          case 4:
            // todo: read SIB byte
            break;
          case 5:
            // todo: read disp32
            break;
          }
          break;
        case 1:
          break;
        case 2:
          break;
        case 3:
          // operand 1 is just the contents of the rm register
          effective_address = &r[rm].i;
          break;
      }
      int arg2 = imm32();
      int64_t tmp = *effective_address + arg2;
      *effective_address += arg2;
      SF = (*effective_address < 0);
      ZF = (*effective_address == 0);
      OF = (*effective_address != tmp);
      break;
    }
    case 0x01: {  // add r/m32, r32
      break;
    }
    case 0x03:  // add r32, r/m32
      break;
    case 0x0f:  // escape
      switch(next()) {
        default:
          fprintf(stderr, "unrecognized second opcode after 0x0f: %x\n", mem[EIP-1]);
          exit(1);
      }
      break;
    case 0xf3:  // escape
      switch(next()) {
        case 0x0f:  // escape
          switch(next()) {
            case 0x58:  // addss xmm, xmm/m32
              break;
            default:
              fprintf(stderr, "unrecognized third opcode after 0xf3 0x0f: %x\n", mem[EIP-1]);
              exit(1);
          }
          break;
        default:
          fprintf(stderr, "unrecognized second opcode after 0xf3: %x\n", mem[EIP-1]);
          exit(1);
      }
      break;
    case 0xf4:  // hlt
      EIP = mem_size;
      break;
    default:
      fprintf(stderr, "unrecognized opcode: %x\n", mem[EIP-1]);
      exit(1);
  }
}

uint8_t next(void) {
  if (EIP >= mem_size) return /*hlt*/0xf4;
  return mem[EIP++];
}

// read a 32-bit immediate in little-endian order from the instruction stream
int imm32(void) {
  int result = next();
  result |= (next()<<8);
  result |= (next()<<16);
  result |= (next()<<24);
  return result;
}

//// more tests

void test_add_imm32_to_r32(void) {
  load_program(
    // opcodes    modrm     sib       displacement      immediate
    "81           c3                                    0a 0b 0c 0d"  // add EBX 0x0d0c0b0a
  );
  run();
  CHECK(r[EBX].u == 0x0d0c0b0a);
}

void test_add_imm32_to_mem_at_r32(void) {
  // EBX starts out as 0
  load_program(
    // opcodes    modrm     sib       displacement      immediate
    "81           03                                    0a 0b 0c 0d"  // add (EBX), 0x0d0c0b0a
  );
  run();
  // Immediate operands at addresses 2-5 are added to memory locations 0-3.
  // Self-modifying code! Just to make the test simple, though. I don't
  // endorse this.
  CHECK(mem[0] == 0x8b);  // 81 + 0a
  CHECK(mem[1] == 0x0e);  // 03 + 0b
  CHECK(mem[2] == 0x16);  // 0a + 0c
  CHECK(mem[3] == 0x18);  // 0b + 0d
}
