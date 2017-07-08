// VM that is a subset of x86

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

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

int r[NUM_INT_REGISTERS] = {0};
float xmm[NUM_FLOAT_REGISTERS] = {0};
unsigned int EIP = 0;
bool OF=false, ZF=false, SF=false;

uint8_t mem[] = {
  // program goes here
  0x05, 0x0a, 0x0b, 0x0c, 0x0d,  // add EAX, 0x0d0c0b0a
  0xf4,  // hlt
};

uint8_t next() {
  if (EIP >= sizeof(mem)) return /*hlt*/0xf4;
  return mem[EIP++];
}

// read a 32-bit immediate in little-endian order from the instruction stream
int imm32() {
  int result = next();
  result |= (next()<<8);
  result |= (next()<<16);
  result |= (next()<<24);
  return result;
}

void run_one_instruction() {
  switch (next()) {
    case 0x05: {  // add EAX, imm32
      int arg2 = imm32(mem);
      int64_t tmp = r[EAX] + arg2;
      r[EAX] += arg2;
      SF = (r[EAX] < 0);
      ZF = (r[EAX] == 0);
      OF = (r[EAX] != tmp);
      break;
    }
    case 0x81:  // add r/m32, imm32
      break;
    case 0x01:  // add r/m32, r32
      break;
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
      fprintf(stderr, "hlt encountered\n");
      exit(0);
    default:
      fprintf(stderr, "unrecognized opcode: %x\n", mem[EIP-1]);
      exit(1);
  }
}

void run() {
  EIP = 0;
  while (EIP < sizeof(mem)) {
    run_one_instruction();
  }
}

int main() {
  // run on a 32-bit system
  assert(sizeof(int) == 4);
  assert(sizeof(float) == 4);
  //
  assert(sizeof(mem) > 1);
  run();
  return 0;
}
