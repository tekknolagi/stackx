# I use this file to test out the encoding of various x86 instructions
# on OpenBSD.
#
# $ as z.openbsd.s -o z.o  &&  objdump --disassemble -Mintel z.o
#
# -Mintel ensures destination is printed first in each instruction

# No binary header since we don't use this file for running.

.intel_syntax
.section .text
.globl _start
_start:
  # destination comes last
  xor %eax, %eax
  # add register to memory address
  add [0x0a0b0c0d], %eax
  add [0x0a0b0c0d], %ebx
  # add immediate value to register
  add %eax, dword ptr 0x0a0b0c0d
  add %ebx, dword ptr 0x0a0b0c0d  # modrm byte is 11_000_011
  add [%ebx], dword ptr 0x0a0b0c0d  # modrm byte is 00_000_011
  add %eax, %ebx
  add %ebx, [%eax]
  add [%eax], %ebx
  sub %eax, [%ebx]
  add %eax, [%ebx+3]
  add [%eax+3], %ebx
  add %eax, [%ebp+%edx*4-4]
  and %ebx, dword ptr 0x0a0b0c0d
  and [%ebx], dword ptr 0x0a0b0c0d
  or %ebx, dword ptr 0x0a0b0c0d
  or [%ebx], dword ptr 0x0a0b0c0d
  or %ebx, [%eax]
  or [%eax], %ebx
  #
  # exit
  xor %eax, %eax
  push %eax  # exit status
  push %eax  # extra long for C ABI
  mov %eax, 0x1  # exit syscall
  int 0x80
