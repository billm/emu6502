import types
import addressing
import flags
import memory
import stack
import strformat
import strutils
import utils

proc updateZNFlags*(cpu: var CPU, value: uint8) =
  ## Updates the Zero and Negative flags based on the provided value.
  cpu.setZ(value)
  cpu.setN(value)

proc performORA*(cpu: var CPU, value: uint8) =
  ## Performs the ORA operation (A = A | value) and updates Z/N flags.
  cpu.A = cpu.A or value
  cpu.updateZNFlags(cpu.A)

proc performASL*(cpu: var CPU, value: uint8): uint8 =
  ## Performs the ASL operation (value << 1), updates Carry flag, and returns the result.
  cpu.C = (value and 0x80'u8) != 0 # Set Carry if bit 7 was set
  result = value shl 1

proc performASLOnMemory*(cpu: var CPU, address: uint16) =
  ## Performs ASL on a memory location, updates memory, and sets Z/N flags.
  let originalValue = cpu.memory[address]
  let shiftedValue = cpu.performASL(originalValue)
  cpu.memory[address] = shiftedValue
  cpu.updateZNFlags(shiftedValue)

proc fetchOperandValue*(cpu: var CPU, info: OpcodeInfo, addrResult: AddressingResult): uint8 =
  ## Fetches the operand value based on the addressing mode.
  if info.mode == immediate:
    addrResult.value
  else:
    cpu.memory[addrResult.address]


proc handleBranch*(cpu: var CPU, info: OpcodeInfo, result: AddressingResult, condition: bool) =
  ## Handles the common logic for branch instructions.
  let pcIncrement = uint16(result.operandBytes + 1)
  let currentPC = cpu.PC
  var cyclesToAdd = uint16(info.cycles)
  var nextPC = currentPC + pcIncrement

  if condition:
    cyclesToAdd += 1 # Add 1 cycle penalty for taking the branch

    # Check for page boundary crossing
    let targetAddress = result.address
    let pageCrossed = (currentPC + pcIncrement) div 256 != targetAddress div 256
    if pageCrossed:
      cyclesToAdd += 1 # Add additional cycle penalty for page cross

    nextPC = targetAddress

  cpu.cycles += cyclesToAdd
  cpu.PC = nextPC

proc updatePCAndCycles*(cpu: var CPU, info: OpcodeInfo, result: AddressingResult) =
  ## Updates the Program Counter and CPU cycles based on opcode info and addressing result.
  cpu.PC += uint16(result.operandBytes + 1)
  if info.fixedCycles:
    cpu.cycles += uint16(info.cycles)
  else:
    cpu.cycles += uint16(info.cycles + result.extraCycles)
