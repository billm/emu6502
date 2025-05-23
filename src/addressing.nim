import types
import memory

export types.OperatorMode

type
  AddressingResult* = object
    address*: uint16  # The final address to read from/write to
    value*: uint8     # For immediate mode, the actual value
    extraCycles*: int # Additional cycles due to page boundary crossing
    operandBytes*: int # Number of bytes this addressing mode uses

proc resolveImmediate*(cpu: var CPU): AddressingResult =
  ## Immediate addressing mode (#$XX)
  ## Example: LDA #$42
  result.value = cpu.memory[cpu.PC + 1]
  result.operandBytes = 1
  result.extraCycles = 0

proc resolveZeroPage*(cpu: var CPU): AddressingResult =
  ## Zero page addressing mode ($XX)
  ## Example: LDA $42
  result.address = cpu.memory[cpu.PC + 1].uint16
  result.operandBytes = 1
  result.extraCycles = 0

proc resolveZeroPageX*(cpu: var CPU): AddressingResult =
  ## Zero page X addressing mode ($XX,X)
  ## Example: LDA $42,X
  let zpAddr = (cpu.memory[cpu.PC + 1] + cpu.X) and 0xFF
  result.address = zpAddr.uint16
  result.operandBytes = 1
  result.extraCycles = 0

proc resolveZeroPageY*(cpu: var CPU): AddressingResult =
  ## Zero page Y addressing mode ($XX,Y)
  ## Example: LDA $42,Y
  let zpAddr = (cpu.memory[cpu.PC + 1] + cpu.Y) and 0xFF
  result.address = zpAddr.uint16
  result.operandBytes = 1
  result.extraCycles = 0

proc resolveAbsolute*(cpu: var CPU): AddressingResult =
  ## Absolute addressing mode ($XXXX)
  ## Example: LDA $1234
  result.address = cpu.memory.read16(cpu.PC + 1)
  result.operandBytes = 2
  result.extraCycles = 0

proc resolveAbsoluteX*(cpu: var CPU): AddressingResult =
  ## Absolute X addressing mode ($XXXX,X)
  ## Example: LDA $1234,X
  let baseAddr = cpu.memory.read16(cpu.PC + 1)
  result.address = baseAddr + cpu.X
  result.operandBytes = 2
  # Add cycle if page boundary crossed
  if (baseAddr and 0xFF00) != (result.address and 0xFF00):
    result.extraCycles = 1
  else:
    result.extraCycles = 0

proc resolveAbsoluteY*(cpu: var CPU): AddressingResult =
  ## Absolute Y addressing mode ($XXXX,Y)
  ## Example: LDA $1234,Y
  let baseAddr = cpu.memory.read16(cpu.PC + 1)
  result.address = baseAddr + cpu.Y
  result.operandBytes = 2
  # Add cycle if page boundary crossed
  if (baseAddr and 0xFF00) != (result.address and 0xFF00):
    result.extraCycles = 1
  else:
    result.extraCycles = 0

proc resolveIndirectX*(cpu: var CPU): AddressingResult =
  ## Indirect X addressing mode ($XX,X)
  ## Example: LDA ($42,X)
  let zpAddr = (cpu.memory[cpu.PC + 1] + cpu.X) and 0xFF
  let indirectAddr = cpu.memory.read16(zpAddr.uint16)
  result.address = indirectAddr
  result.operandBytes = 1
  result.extraCycles = 0

proc resolveIndirectY*(cpu: var CPU): AddressingResult =
  ## Indirect Y addressing mode ($XX),Y
  ## Example: LDA ($42),Y
  let zpAddr = cpu.memory[cpu.PC + 1]
  # Handle zero-page wrap-around for the 16-bit address fetch
  let lowByte = cpu.memory[zpAddr.uint16]
  let highByte = cpu.memory[((zpAddr + 1) and 0xFF).uint16] # Wrap high byte address
  let baseAddr = (highByte.uint16 shl 8) or lowByte.uint16
  result.address = baseAddr + cpu.Y
  result.operandBytes = 1
  # Add cycle if page boundary crossed
  if (baseAddr and 0xFF00) != (result.address and 0xFF00):
    result.extraCycles = 1
  else:
    result.extraCycles = 0

proc resolveRelative*(cpu: var CPU): AddressingResult =
  ## Relative addressing mode ($XX)
  ## Example: BNE $42
  let offset = cpu.memory[cpu.PC + 1]
  result.operandBytes = 1
  result.extraCycles = 0

  # Relative branch target is PC+2 plus signed offset
  let nextPC = cpu.PC + 2
  # Calculate target address
  let signed_offset = cast[int8](offset)  # Always treat as signed
  result.address = uint16(int(nextPC) + int(signed_offset))
  # Add cycle if page boundary crossed
  if (cpu.PC and 0xFF00) != (result.address and 0xFF00):
    result.extraCycles = 1

proc resolveAddressingMode*(cpu: var CPU, mode: OperatorMode): AddressingResult =
  ## Main entry point for resolving addressing modes
  case mode
  of immediate:
    result = resolveImmediate(cpu)
  of zeroPage:
    result = resolveZeroPage(cpu)
  of zeroPageX:
    result = resolveZeroPageX(cpu)
  of zeroPageY:
    result = resolveZeroPageY(cpu)
  of absolute:
    result = resolveAbsolute(cpu)
  of absoluteX:
    result = resolveAbsoluteX(cpu)
  of absoluteY:
    result = resolveAbsoluteY(cpu)
  of indirect:
    # Not implemented in 6502
    raise newException(ValueError, "Indirect addressing mode not implemented")
  of indirectX:
    result = resolveIndirectX(cpu)
  of indirectY:
    result = resolveIndirectY(cpu)
  of implied:
    discard # Should not be called for implied mode

  of accumulator:
    discard # Should not be called for accumulator mode

  of relative:
    result = resolveRelative(cpu) 