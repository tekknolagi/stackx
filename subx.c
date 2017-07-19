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

// make type coercions easier to spot
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
uint32_t EIP = 0;
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
    // opcode     modrm     sib       displacement      immediate
    "05                                                 0a 0b 0c 0d "  // add EAX 0x0d0c0b0a
  );
  run();
  CHECK(r[EAX].u == 0x0d0c0b0a);
}

// load the given space-separated sequence of hex bytes into `mem` starting at
// address 0.
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

// beware: no side-effects in args
#define PERFORM_ARITHMETIC_BINOP(op, arg1, arg2) { \
  /* arg1 and arg2 must be signed */ \
  int64_t tmp = arg1 op arg2; \
  arg1 = arg1 op arg2; \
  SF = (arg1 < 0); \
  ZF = (arg1 == 0); \
  OF = (arg1 != tmp); \
}

void run_one_instruction() {
  switch (next()) {
    case 0x01: {  // add r/m32, r32
      uint8_t modrm = next();
      int32_t* arg1 = effective_address(modrm);
      uint8_t arg2 = (modrm>>3)&0x7;
      PERFORM_ARITHMETIC_BINOP(+, *arg1, r[arg2].i);
      break;
    }
    case 0x03: {  // add r32, r/m32
      uint8_t modrm = next();
      const int32_t* arg2 = effective_address(modrm);
      uint8_t arg1 = (modrm>>3)&0x7;
      PERFORM_ARITHMETIC_BINOP(+, r[arg1].i, *arg2);
      break;
    }
    case 0x05: {  // add EAX, imm32
      int32_t arg2 = imm32();
      PERFORM_ARITHMETIC_BINOP(+, r[EAX].i, arg2);
      break;
    }
    case 0x0f:  // escape
      switch(next()) {
        default:
          fprintf(stderr, "unrecognized second opcode after 0x0f: %x\n", mem[EIP-1]);
          exit(1);
      }
      break;
    case 0x81: {  // add r/m32, imm32
      uint8_t modrm = next();
      int32_t* arg1 = effective_address(modrm);
      int32_t arg2 = imm32();
      uint8_t subop = (modrm>>3)&0x7;
      switch (subop) {
        case 0:
          PERFORM_ARITHMETIC_BINOP(+, *arg1, arg2);
          break;
        case 5:
          PERFORM_ARITHMETIC_BINOP(-, *arg1, arg2);
          break;
      }
      break;
    }
    case 0x90:  // nop (xchg eax, eax)
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
int32_t imm32(void) {
  int32_t result = next();
  result |= (next()<<8);
  result |= (next()<<16);
  result |= (next()<<24);
  return result;
}

// Implement tables 2-2 and 2-3 in the Intel manual, Volume 2.
int32_t* effective_address(uint8_t modrm) {
  uint8_t mod = (modrm>>6);
  // ignore middle 3 'reg opcode' bits
  uint8_t rm = modrm & 0x7;
  int32_t* result = 0;
  switch (mod) {
    case 0:
      // mod 0 is usually indirect addressing
      switch (rm) {
      default:
//?         printf("%hhx %u %u\n", rm, r[rm].u, mem_size);
        assert(r[rm].u + sizeof(int32_t) <= mem_size);
        result = CAST(int32_t*, &mem[r[rm].u]);  // rely on the host itself being in little-endian order
        break;
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
      result = &r[rm].i;
      break;
  }
  return result;
}

//// === more tests

//// add

// test_add_imm32_to_eax is above as the example test

// add with mod = 11 (register direct mode)
void test_add_imm32_to_rm32(void) {
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "81           c3                                    0a 0b 0c 0d "  // add EBX, 0x0d0c0b0a
  );
  run();
  CHECK(r[EBX].u == 0x0d0c0b0a);
}

// add with mod = 00 (register indirect mode)
void test_add_imm32_to_mem_at_rm32(void) {
  // EBX starts out as 0
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "81           03                                    0a 0b 0c 0d "  // add (EBX), 0x0d0c0b0a
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

void test_add_r32_to_rm32(void) {
  r[EBX].u = 0x10;
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "01           18 "  // add (EAX), EBX
    "90 90"  // padding
  );
  run();
  CHECK(mem[0] == 0x11);  // 0x01 + 0x10
  CHECK(mem[1] == 0x18);  // unchanged
  CHECK(mem[2] == 0x90);  // unchanged
  CHECK(mem[3] == 0x90);  // unchanged
}

void test_add_rm32_to_r32(void) {
  r[EBX].u = 0x01;
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "03           18 "  // add EBX, (EAX)
    "90 90"  // padding
  );
  run();
  CHECK(r[EBX].u == 0x90901804);
}

//// sub

// subtract with mod = 11 (register direct mode)
void test_sub_imm32_to_rm32(void) {
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "81           eb                                    01 00 00 00 "  // sub EBX, 0x01
  );
  run();
  CHECK(r[EBX].u == 0xffffffff);  // -1
}

// subtract with mod = 00 (register indirect mode)
void test_sub_imm32_to_mem_at_rm32(void) {
  // EBX starts out as 0
  load_program(
    // opcode     modrm     sib       displacement      immediate
    "81           2b                                    01 00 00 00 "  // sub (EBX), 0x01
  );
  run();
  // Immediate operands at addresses 2-5 are subtracted from memory locations
  // 0-3. Self-modifying code! Just to make the test simple, though. I don't
  // endorse this.
  CHECK(mem[0] == 0x80);  // 81 - 1
  CHECK(mem[1] == 0x2b);  // unchanged
  CHECK(mem[2] == 0x01);  // unchanged
  CHECK(mem[3] == 0x00);  // unchanged
  CHECK(mem[4] == 0x00);  // unchanged
  CHECK(mem[5] == 0x00);  // unchanged
}
