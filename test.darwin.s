# I use this file to test out the encoding of various x86 instructions
# on Mac OS.
#
# $ as -arch i386 test.darwin.s -o test.o  &&  objdump --disassemble test.o

# No binary header since we don't use this file for running.

.section __TEXT, __text
.globl _start
_start:
  xor %eax, %eax
  # surprising that 'add r32, imm32' encodes to
  #   01 05 <imm32>
  # rather than
  #   81 ...
  add %eax, 0x0a0b0c0d
  #
  # exit
  xor %eax, %eax
  push %eax  # exit status
  push %eax  # extra long for C ABI
  mov $1, %eax  # exit syscall
  int $0x80
