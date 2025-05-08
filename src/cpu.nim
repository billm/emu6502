import strformat
import strutils

import types
import memory
import opcodes
import utils
import flags


export types.CPU
export types.Memory

proc step*(cpu: var CPU) =
  ## Executes a single CPU instruction cycle.
  let opcode = cpu.memory[cpu.PC]
  let info = opcodeTable[opcode]
  let oldPC = cpu.PC # Save PC for detecting BRK or halt conditions

  if info.handler == nil:
    raise UnimplementedOpcodeError(opcode: opcode, pc: cpu.PC, msg: &"Unimplemented opcode: {opcode.toHex} @ PC: 0x{cpu.PC.toHex}")

  info.handler(cpu, info)

  cpu.debug() # Optional: keep debug output per step

  # Check for halt conditions
  if cpu.PC == 0x8000 and cpu.memory[cpu.PC] == 0x00:
    cpu.halted = true


# Execute the opcode bytestream
proc execute*(cpu: var CPU) =

  cpu.halted = false

  # Execute instructions until halted
  while not cpu.halted:
    cpu.step()


# Set up the CPU, registers and memory as it should be
# on initialization or reset
proc initialize*(cpu: var CPU, mem: Memory, PC: uint16) =
  cpu.A = 0x0
  cpu.X = 0x0
  cpu.Y = 0x0
  cpu.C = false
  cpu.Z = false # Initialize Z flag
  cpu.I = false # Initialize I flag (clear by default)
  cpu.D = false # Initialize D flag
  cpu.B = false # Initialize B flag
  cpu.U = true  # Initialize U flag (unused, reads as 1)
  cpu.V = false # Initialize V flag
  cpu.N = false # Initialize N flag
  cpu.SP = 0xff
  cpu.PC = PC
  cpu.cycles = 0
  cpu.halted = false
  cpu.memory = mem
  # echo "Initializing first 8 bytes of memory to 0xfeedface 0xdeadbeef"
  cpu.memory[0] = 0xfe
  cpu.memory[1] = 0xed
  cpu.memory[2] = 0xfa
  cpu.memory[3] = 0xce
  cpu.memory[4] = 0xde
  cpu.memory[5] = 0xad
  cpu.memory[6] = 0xbe
  cpu.memory[7] = 0xef


