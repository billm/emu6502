import strformat
import strutils

import types
import memory
import opcodes
import utils
import flags


export types.CPU
export types.Memory

# Execute the opcode bytestream
proc execute*(cpu: var CPU) =
  #echo "Registers initialized as:"
  #cpu.debug(true)

  while true:
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    let oldPC = cpu.PC  # Save PC for detecting BRK
    
    if info.handler == nil:
      raise UnimplementedOpcodeError(opcode: opcode, pc: cpu.PC, msg: &"Unimplemented opcode: {opcode.toHex} @ PC: 0x{cpu.PC.toHex}")
    
    info.handler(cpu)
    cpu.debug()

    # Exit if we hit BRK (which doesn't update PC in its handler)
    if opcode == 0x00 and oldPC == cpu.PC:
      break

  #echo "\nAt program exit, the CPU state is:"
  #cpu.debug(true)
  #echo "Done"

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


proc step*(cpu: var CPU): bool =
  ## Executes a single CPU instruction cycle.
  ## Returns true if execution should continue, false if BRK occurred.
  let opcode = cpu.memory[cpu.PC]
  let info = opcodeTable[opcode]
  let oldPC = cpu.PC # Save PC for detecting BRK or halt conditions

  if info.handler == nil:
    raise UnimplementedOpcodeError(opcode: opcode, pc: cpu.PC, msg: &"Unimplemented opcode: {opcode.toHex} @ PC: 0x{cpu.PC.toHex}")
  echo &"[DEBUG] BEFORE PC={cpu.PC.toHex(4)} OP={opcode.toHex(2)} A={cpu.A.toHex(2)} X={cpu.X.toHex(2)} Y={cpu.Y.toHex(2)} P={cpu.flags.toHex(2)} SP={cpu.SP.toHex(2)}"

  info.handler(cpu)
  echo &"[DEBUG] AFTER  PC={cpu.PC.toHex(4)} OP={opcode.toHex(2)} A={cpu.A.toHex(2)} X={cpu.X.toHex(2)} Y={cpu.Y.toHex(2)} P={cpu.flags.toHex(2)} SP={cpu.SP.toHex(2)}"
  cpu.debug() # Optional: keep debug output per step

  # Check for halt conditions (like KIL or BRK not advancing PC)
  if cpu.halted or (opcode == 0x00 and oldPC == cpu.PC):
    return false # Signal to stop execution

  return true # Signal to continue execution
