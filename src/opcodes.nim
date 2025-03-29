import types
import addressing
import utils
import strformat
import strutils
import stack
import memory
import flags

export types.CPU
export types.OperatorMode

type
  OpcodeHandler* = proc(cpu: var CPU)
  OpcodeInfo* = object
    handler*: OpcodeHandler
    cycles*: int
    mode*: OperatorMode
    mnemonic*: string

var opcodeTable*: array[256, OpcodeInfo]

proc opBRK(cpu: var CPU) =
  cpu.printOpCode("BRK")
  
  # Save return address before other operations
  let returnAddr = cpu.PC + 2'u16  # BRK pushes PC+2
  
  # Set B flag and get status before pushing
  cpu.B = true  # Set break flag before pushing status
  let statusToPush = cpu.flags()  # Get status while B is set
  cpu.push16(returnAddr)  # Push return address (PC+2: high byte, then low byte)
  cpu.push(statusToPush)  # Push processor status (with B flag set)
  cpu.B = false  # Clear B flag in actual CPU status
  cpu.I = true  # Set interrupt disable flag
  cpu.PC = (cpu.memory[0xFFFE].uint16 or (cpu.memory[0xFFFF].uint16 shl 8))  # Load IRQ vector
  cpu.cycles += 7  # BRK takes 7 cycles

proc opJSR(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, absolute)
  cpu.printOpCode(result.address, &"JSR ${result.address.toHex:04}")
  cpu.push16(cpu.PC + 2)
  cpu.PC = result.address
  cpu.cycles += 6  # JSR takes 6 cycles

proc opRTS(cpu: var CPU) =
  cpu.printOpCode("RTS")
  cpu.PC = cpu.pull16() + 1
  cpu.cycles += 6  # RTS takes 6 cycles

proc opSTY(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"STY ${result.address.toHex:02}")
  cpu.memory[result.address] = cpu.Y
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # STY zp takes 3 cycles

proc opSTA(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"STA ${result.address.toHex:02}")
  cpu.memory[result.address] = cpu.A
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # STA zp takes 3 cycles

proc opSTX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"STX ${result.address.toHex:02}")
  cpu.memory[result.address] = cpu.X
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # STX zp takes 3 cycles

proc opLDY(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, immediate)
  cpu.printOpCode(result.value, &"LDY ${result.value.toHex:02}")
  cpu.Y = result.value
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 2  # LDY immediate takes 2 cycles
  cpu.setZ(cpu.Y)
  cpu.setN(cpu.Y)

proc opLDA_indirectX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, indirectX)
  cpu.printOpCode(result.address, &"LDA ({result.address.toHex:02}, X)")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 6 + uint16(result.extraCycles)  # LDA (zp,X) takes 6 cycles
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)


proc opORA_indirectX*(cpu: var CPU) =
  ## ORA (Indirect,X) - Opcode 0x01
  let result = resolveAddressingMode(cpu, indirectX)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {result.address.toHex:04} = {value.toHex:02}")

  cpu.A = cpu.A or value
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 6 + uint16(result.extraCycles) # ORA (Indirect,X) takes 6 cycles


proc opSLO_indirectX*(cpu: var CPU) =
  ## SLO (Indirect,X) - Opcode 0x03 (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, indirectX)
  let addr = result.address
  let originalValue = cpu.memory[addr]
  cpu.printOpCode(addr, &"SLO (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {addr.toHex:04} = {originalValue.toHex:02}")

  # 1. ASL on M
  cpu.C = (originalValue and 0x80'u8) != 0 # Set Carry if bit 7 was set
  let shiftedValue = originalValue shl 1

  # 2. Write shifted value back to memory
  cpu.memory[addr] = shiftedValue

  # 3. ORA with Accumulator
  cpu.A = cpu.A or shiftedValue
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 8 + uint16(result.extraCycles) # SLO (Indirect,X) takes 8 cycles

proc opLDX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, immediate)
  cpu.printOpCode(result.value, &"LDX ${result.value.toHex:02}")
  cpu.X = result.value
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 2  # LDX immediate takes 2 cycles
  cpu.setZ(cpu.X)
  cpu.setN(cpu.X)

proc opLDY_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDY ${result.address.toHex:02}")
  cpu.Y = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDY zp takes 3 cycles
  cpu.setZ(cpu.Y)
  cpu.setN(cpu.Y)

proc opLDA_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDA ${result.address.toHex:02}")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDA zp takes 3 cycles
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

proc opLDX_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDX ${result.address.toHex:02}")
  cpu.X = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDX zp takes 3 cycles
  cpu.setZ(cpu.X)
  cpu.setN(cpu.X)

proc opLDA_imm(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, immediate)
  cpu.printOpCode(result.value, &"LDA ${result.value.toHex:02}")
  cpu.A = result.value
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 2  # LDA immediate takes 2 cycles
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

proc opLDA_indirectY(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, indirectY)
  cpu.printOpCode(result.address, &"LDA ({result.address.toHex:04}), Y")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 5 + uint16(result.extraCycles)  # LDA (zp),Y takes 5 cycles + 1 if page crossed
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

proc opLDA_absX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, absoluteX)
  cpu.printOpCode(result.address, &"LDA ${result.address.toHex:04}, X")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 4 + uint16(result.extraCycles)  # LDA abs,X takes 4 cycles + 1 if page crossed
  cpu.setZ(cpu.A)
  cpu.setN(cpu.A)

proc opBNE(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, relative)
  cpu.printOpCode(result.address, &"BNE ${result.address.toHex:02}")
  if not cpu.Z:
    cpu.PC = result.address
    cpu.cycles += 2 + uint16(result.extraCycles)  # BNE takes 2 cycles + 1 if page crossed
  else:
    cpu.PC += uint16(result.operandBytes + 1)
    cpu.cycles += 2  # BNE not taken takes 2 cycles

proc opINX(cpu: var CPU) =
  cpu.printOpCode("INX")
  cpu.X += 1
  cpu.PC += 1
  cpu.cycles += 2  # INX takes 2 cycles
  cpu.setZ(cpu.X)
  cpu.setN(cpu.X)

proc opBEQ(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, relative)
  cpu.printOpCode(result.address, &"BEQ ${result.address.toHex:02}")
  if cpu.Z:
    cpu.PC = result.address
    cpu.cycles += 2 + uint16(result.extraCycles)  # BEQ takes 2 cycles + 1 if page crossed
  else:
    cpu.PC += uint16(result.operandBytes + 1)
    cpu.cycles += 2  # BEQ not taken takes 2 cycles

proc opKIL(cpu: var CPU) =
  ## KIL Implied - Opcode 0x02 (unofficial)
  ## Halts the processor.
  cpu.printOpCode("KIL")
  cpu.halted = true
  cpu.PC += 1 # KIL is a 1-byte instruction
  cpu.cycles += 2 # Nominal cycle count for KIL



proc opNOP_zp*(cpu: var CPU) =
  ## NOP ZeroPage - Opcode 0x04 (unofficial)
  ## Action: Fetches operand, reads from ZeroPage address, discards value.
  let result = resolveAddressingMode(cpu, zeroPage)
  # The read happens implicitly within resolveAddressingMode for zp
  cpu.printOpCode(result.address, &"NOP ${result.address.toHex:02}")

  # No operation performed with the value read
  # No flags or registers are affected

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 3 # NOP zp takes 3 cycles


proc setupOpcodeTable*() =
  # Initialize all opcodes to nil
  opcodeTable[0x02] = OpcodeInfo(handler: opKIL, mode: immediate, cycles: 2, mnemonic: "KIL") # Unofficial
  for i in 0..255:
    opcodeTable[i].handler = nil
    opcodeTable[i].cycles = 0
    opcodeTable[i].mode = immediate
    opcodeTable[i].mnemonic = "???"

  # Set up known opcodes
  opcodeTable[0x00] = OpcodeInfo(handler: opBRK, mode: immediate, cycles: 7, mnemonic: "BRK")
  opcodeTable[0x01] = OpcodeInfo(handler: opORA_indirectX, mode: indirectX, cycles: 6, mnemonic: "ORA")
  opcodeTable[0x03] = OpcodeInfo(handler: opSLO_indirectX, mode: indirectX, cycles: 8, mnemonic: "SLO") # Unofficial
  opcodeTable[0x04] = OpcodeInfo(handler: opNOP_zp, mode: zeroPage, cycles: 3, mnemonic: "NOP") # Unofficial

  opcodeTable[0x20] = OpcodeInfo(handler: opJSR, mode: absolute, cycles: 6, mnemonic: "JSR")
  opcodeTable[0x60] = OpcodeInfo(handler: opRTS, mode: immediate, cycles: 6, mnemonic: "RTS")
  opcodeTable[0x84] = OpcodeInfo(handler: opSTY, mode: zeroPage, cycles: 3, mnemonic: "STY")
  opcodeTable[0x85] = OpcodeInfo(handler: opSTA, mode: zeroPage, cycles: 3, mnemonic: "STA")
  opcodeTable[0x86] = OpcodeInfo(handler: opSTX, mode: zeroPage, cycles: 3, mnemonic: "STX")
  opcodeTable[0xa0] = OpcodeInfo(handler: opLDY, mode: immediate, cycles: 2, mnemonic: "LDY")
  opcodeTable[0xa1] = OpcodeInfo(handler: opLDA_indirectX, mode: indirectX, cycles: 6, mnemonic: "LDA")
  opcodeTable[0xa2] = OpcodeInfo(handler: opLDX, mode: immediate, cycles: 2, mnemonic: "LDX")
  opcodeTable[0xa4] = OpcodeInfo(handler: opLDY_zp, mode: zeroPage, cycles: 3, mnemonic: "LDY")
  opcodeTable[0xa5] = OpcodeInfo(handler: opLDA_zp, mode: zeroPage, cycles: 3, mnemonic: "LDA")
  opcodeTable[0xa6] = OpcodeInfo(handler: opLDX_zp, mode: zeroPage, cycles: 3, mnemonic: "LDX")
  opcodeTable[0xa9] = OpcodeInfo(handler: opLDA_imm, mode: immediate, cycles: 2, mnemonic: "LDA")
  opcodeTable[0xb1] = OpcodeInfo(handler: opLDA_indirectY, mode: indirectY, cycles: 5, mnemonic: "LDA")
  opcodeTable[0xbd] = OpcodeInfo(handler: opLDA_absX, mode: absoluteX, cycles: 4, mnemonic: "LDA")
  opcodeTable[0xd0] = OpcodeInfo(handler: opBNE, mode: relative, cycles: 2, mnemonic: "BNE")
  opcodeTable[0xe8] = OpcodeInfo(handler: opINX, mode: immediate, cycles: 2, mnemonic: "INX")
  opcodeTable[0xf0] = OpcodeInfo(handler: opBEQ, mode: relative, cycles: 2, mnemonic: "BEQ") 