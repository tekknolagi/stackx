# I use this file to test out the encoding of various x86 instructions
# on Mac OS.
#
# $ as -arch i386 z.darwin.s -o z.o  &&  objdump --disassemble -x86-asm-syntax=intel z.o
#
# -x86-asm-syntax=intel ensures destination is printed first in each instruction

# No binary header since we don't use this file for running.

.section __TEXT, __text
.globl _start
_start:
  # destination comes last
  xor %eax, %eax
  add %eax, 0x0a0b0c0d
  add (%ebx), %eax
  add %ebx, (%eax)
  sub (%ebx), %eax
  #
  # exit
  xor %eax, %eax
  push %eax  # exit status
  push %eax  # extra long for C ABI
  mov $1, %eax  # exit syscall
  int $0x80
