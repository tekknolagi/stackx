// VM that is a subset of x86

#include <stdint.h>
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
int EIP = 0;

void run_one_instruction(uint8_t mem[]) {
  switch (mem[EIP++]) {
    case 0x05:  // add EAX, imm32
      break;
    case 0x81:  // add r/m32, imm32
      break;
    case 0x01:  // add r/m32, r32
      break;
    case 0x03:  // add r32, r/m32
      break;
    case 0xf3:  // escape
      switch(mem[EIP++]) {
        case 0x0f:  // escape
          switch(mem[EIP++]) {
            case 0x58:  // addss xmm, xmm/m32
              break;
            default:
              assert(false);
          }
          break;
        default:
          assert(false);
      }
      break;
    case 0x0f:  // escape
      switch(mem[EIP++]) {
        default:
          assert(false);
      }
      break;
    default:
      assert(false);
  }
}

void run(uint8_t mem[]) {
  EIP = 0;
  while (true) {
    run_one_instruction(mem);
  }
}

int main() {
  // run on a 32-bit system
  assert(sizeof(int) == 4);
  assert(sizeof(float) == 4);
  uint8_t mem[] = {
    // program goes here
  };
  return 0;
}
