import types
import memory
import strutils

# Push a byte onto the stack
proc push*(cpu: var CPU, val: uint8) =
  let writeAddr = cpu.SP.uint16 or 0x100
  cpu.memory[writeAddr] = val
  dec cpu.SP

# Pull a byte from the stack
proc pull*(cpu: var CPU): uint8 =
  inc cpu.SP
  result = cpu.memory[cpu.SP.uint16 or 0x100]

# Push a 16-bit value onto the stack
proc push16*(cpu: var CPU, val: uint16) =
  cpu.push uint8(val shr 8)
  cpu.push uint8(val)

# Pull a 16-bit value from the stack
proc pull16*(cpu: var CPU): uint16 =
  uint16(cpu.pull()) or (uint16(cpu.pull()) shl 8) 