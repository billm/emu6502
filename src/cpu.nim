import strformat
import strutils

import types
import memory
export types.CPU
export types.Memory


# Return a specific bit from provided uint8
proc bit(val: uint8, bit: range[0..7]): bool =
  ((val shr bit) and 1) != 0


# Collapse the individual CPU flags into a single 8bit field
proc flags*(cpu: CPU): uint8 =
  cpu.C.uint8 or
    (cpu.Z.uint8 shl 1) or
    (cpu.I.uint8 shl 2) or
    (cpu.D.uint8 shl 3) or
    (cpu.B.uint8 shl 4) or
    (cpu.U.uint8 shl 5) or
    (cpu.V.uint8 shl 6) or
    (cpu.N.uint8 shl 7)


# Set each processor flag boolean based # on the bits set in cpu.flags
proc `flags=`*(cpu: var CPU, flags: uint8) =
  cpu.C = flags.bit(0)
  cpu.Z = flags.bit(1)
  cpu.I = flags.bit(2)
  cpu.D = flags.bit(3)
  cpu.B = flags.bit(4)
  cpu.U = flags.bit(5)
  cpu.V = flags.bit(6)
  cpu.N = flags.bit(7)


# Generic debugging
proc debug*(cpu: CPU) =
  echo "===REG==="
  echo &"A: 0x{toHex(cpu.A)} X: 0x{toHex(cpu.X)} Y: 0x{toHex(cpu.Y)}"
  echo &"PC: 0x{toHex(cpu.PC)} SP: 0x{toHex(cpu.SP)}"

  echo "8 bytes of RAM at memory address 0000:"
  echo &"0001: {cpu.memory[0].toHex} {cpu.memory[1].toHex} {cpu.memory[2].toHex} {cpu.memory[3].toHex}  {cpu.memory[4].toHex} {cpu.memory[5].toHex} {cpu.memory[6].toHex} {cpu.memory[7].toHex}"
  echo "========="


proc printOpCode(cpu: CPU, val: uint8, assembly: string) =
  echo &"{cpu.PC.toHex}: {cpu.memory[cpu.PC].toHex} {val.toHex}   {assembly}"

proc printOpCode(cpu: CPU, val: uint16, assembly: string) =
  echo &"{cpu.PC.toHex}: {cpu.memory[cpu.PC].toHex} {val.toHex} {assembly}"


# Execute the opcode bytestream
proc execute*(cpu: var CPU) =

  let mem = cpu.memory

  echo "Registers initialized as:"
  cpu.debug

  while true:
#    echo &"PC = {cpu.PC:04}"
    case mem[cpu.PC]
    of 0x84:
      # STY - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"STY ${loc:02}")
      mem[loc] = cpu.Y
      cpu.PC += 2
    of 0x85:
      # STA - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"STA ${loc:02}")
      mem[loc] = cpu.A
      cpu.PC += 2
    of 0x86:
      # STX - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"STX ${loc:02}")
      mem[loc] = cpu.X
      cpu.PC += 2
    of 0xa0:
      # LDY - immediate
      let val = mem[cpu.PC+1]
      cpu.printOpCode(val, &"LDY ${val:02}")
      cpu.Y = val
      cpu.PC += 2
    of 0xa2:
      # LDX - immediate
      let val = mem[cpu.PC+1]
      cpu.printOpCode(val, &"LDX ${val:02}")
      cpu.X = val
      cpu.PC += 2
    of 0xa4:
      # LDY - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"LDY ${loc:02}")
      cpu.Y = mem[loc]
      cpu.PC += 2
    of 0xa5:
      # LDA - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"LDA ${loc:02}")
      cpu.A = mem[loc]
      cpu.PC += 2
    of 0xa6:
      # LDX - zeropage
      let loc = mem[cpu.PC+1]
      cpu.printOpCode(loc, &"LDX ${loc:02}")
      cpu.X = mem[loc]
      cpu.PC += 2
    of 0xa9:
      # LDA - immediate
      let val = mem[cpu.PC+1]
      cpu.printOpCode(val, &"LDA ${val:02}")
      cpu.A = val
      cpu.PC += 2
    of 0xbd:
      # LDA - absolute, X
      # TODO wtf am I supposed to do with X? 
      let loc = mem.read16(cpu.PC+1)
      cpu.printOpCode(loc, &"LDA ${loc:04}, X")
      cpu.A = mem[loc]
      cpu.PC += 3

    else:
      echo &"Unknown opcode: {mem[cpu.PC].toHex} @ PC: 0x{cpu.PC.toHex}"
      echo "Exiting..."
      break

  debug(cpu)
  echo "Done"

# Set up the CPU, registers and memory as it should be
# on initialization or reset
proc initialize*(cpu: var CPU, mem: Memory, PC: uint16) =
  cpu.A = 0x41
  cpu.X = 0x42
  cpu.Y = 0x43
  cpu.C = false
  cpu.PC = PC
  cpu.memory = mem
  echo "Initializing first 8 bytes of memory to 0xfeedface 0xdeadbeef"
  cpu.memory[0] = 0xfe
  cpu.memory[1] = 0xed
  cpu.memory[2] = 0xfa
  cpu.memory[3] = 0xce
  cpu.memory[4] = 0xde
  cpu.memory[5] = 0xad
  cpu.memory[6] = 0xbe
  cpu.memory[7] = 0xef
