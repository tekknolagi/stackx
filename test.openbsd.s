# I use this file to test out the encoding of various x86 instructions
# on OpenBSD.
#
# $ as test.openbsd.s -o test.o  &&  objdump --disassemble test.o

# No binary header since we don't use this file for running.

.section .text
.globl _start
_start:
  # destination comes last
  xor %eax, %eax
  # add register to memory address
  add %eax, 0x0a0b0c0d
  add %ebx, 0x0a0b0c0d
  # add immediate value to register
  add $0x0a0b0c0d, %eax
  add $0x0a0b0c0d, %ebx  # modrm byte is 11_000_011
  add %ebx, %eax
  add (%ebx), %eax
  add %ebx, (%eax)
  add 3(%ebx), %eax
  add %ebx, 3(%eax)
  add -4(%ebp, %edx, 4), %eax
  #
  # exit
  xor %eax, %eax
  push %eax  # exit status
  push %eax  # extra long for C ABI
  mov $1, %eax  # exit syscall
  int $0x80
