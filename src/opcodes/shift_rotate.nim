import ../types except OperatorMode # Import types, excluding OperatorMode
import ../types as types_module # Import types again with an alias
import ../addressing
import ../flags
import ../opcode_utils
import ../memory
import std/sugar # For `=>`
import strutils  # For toHex

export types_module.OperatorMode # Export OperatorMode from the aliased module

proc opLSR*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for LSR (Logical Shift Right) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode LSR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err

proc opASL*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic ASL handler (Arithmetic Shift Left).
  ## Action: M = M << 1 or A = A << 1
  let result = resolveAddressingMode(cpu, info.mode)
  cpu.cycles += uint16(result.extraCycles)

  var value: byte
  if info.mode == types_module.OperatorMode.accumulator: # Use aliased OperatorMode
    value = cpu.A
  else:
    value = cpu.memory[result.address]
    # No extra cycles for read-modify-write here, as it's handled in the opcode table

  # Set Carry flag based on bit 7 before shift
  cpu.C = (value and 0b1000_0000) != 0

  # Perform the shift
  value = value shl 1

  # Update the value
  if info.mode == types_module.OperatorMode.accumulator: # Use aliased OperatorMode
    cpu.A = value
  else:
    cpu.memory[result.address] = value

  # Set Zero and Negative flags
  opcode_utils.updateZNFlags(cpu, value)
  # Update PC and CPU cycles
  opcode_utils.updatePCAndCycles(cpu, info, result)

proc opROL*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for ROL (Rotate Left) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ROL ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opROR*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for ROR (Rotate Right) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ROR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err