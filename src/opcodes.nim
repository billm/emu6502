import types
import addressing
import utils # For read16
import strformat
import strutils
import stack
import memory
import flags

export types.CPU
export types.OperatorMode # AddressResult is implicitly available via 'import addressing'

type
  OpcodeHandler* = proc(cpu: var CPU)
  OpcodeInfo* = object
    handler*: OpcodeHandler
    cycles*: int
    mode*: OperatorMode
    mnemonic*: string

var opcodeTable*: array[256, OpcodeInfo]

# Helper Procedures

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

proc performBranch*(cpu: var CPU, condition: bool, result: AddressingResult) =
  ## Handles the logic for conditional branches (PC and cycle updates).
  if condition:
    # Branch taken: base cycles + 1 + page cross penalty
    cpu.cycles += 1 + uint16(result.extraCycles)
    cpu.PC = result.address
  else:
    # Branch not taken: base cycles only
    # PC increment handled outside this proc by caller
    discard

# Opcode Implementations

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
  cpu.PC = read16(cpu.memory, 0xFFFE) # Load IRQ vector address
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
  cpu.updateZNFlags(cpu.Y)

proc opLDA_indirectX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, indirectX)
  cpu.printOpCode(result.address, &"LDA ({result.address.toHex:02}, X)")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 6 + uint16(result.extraCycles)  # LDA (zp,X) takes 6 cycles
  cpu.updateZNFlags(cpu.A)


proc opORA_indirectX*(cpu: var CPU) =
  ## ORA (Indirect,X) - Opcode 0x01
  let result = resolveAddressingMode(cpu, indirectX)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {result.address.toHex:04} = {value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 6 + uint16(result.extraCycles) # ORA (Indirect,X) takes 6 cycles


proc opSLO_indirectX*(cpu: var CPU) =
  ## SLO (Indirect,X) - Opcode 0x03 (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, indirectX)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"SLO (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  # performASLOnMemory handles shift, write back, and Z/N flags based on shifted value
  # We need the shifted value for the ORA step. Let's adjust performASLOnMemory or do it manually here.
  # Manual approach for now to keep helpers simple:
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 8 + uint16(result.extraCycles) # SLO (Indirect,X) takes 8 cycles


proc opORA_zp*(cpu: var CPU) =
  ## ORA ZeroPage - Opcode 0x05
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, zeroPage)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA ${result.address.toHex:02} = {value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 3 # ORA ZeroPage takes 3 cycles


proc opASL_zp*(cpu: var CPU) =
  ## ASL ZeroPage - Opcode 0x06
  ## Action: M = M << 1
  let result = resolveAddressingMode(cpu, zeroPage)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"ASL ${effectiveAddr.toHex:02} = {cpu.memory[effectiveAddr].toHex:02}")

  cpu.performASLOnMemory(effectiveAddr) # Handles shift, write back, C, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 5 # ASL ZeroPage takes 5 cycles
 


proc opBIT_24*(cpu: var CPU) =
  ## BIT ZeroPage - Opcode 0x24
  ## Action: Tests bits in memory with accumulator (A & M).
  ## Flags: Z is set if A & M is zero. N gets M bit 7. V gets M bit 6.
  let result = resolveAddressingMode(cpu, zeroPage)
  let effectiveAddr = result.address
  let value = cpu.memory[effectiveAddr]
  cpu.printOpCode(effectiveAddr, &"BIT ${effectiveAddr.toHex:02} = {value.toHex:02}")

  # 1. Perform A & M to determine Zero flag
  let testResult = cpu.A and value
  cpu.Z = (testResult == 0)

  # 2. Set N and V flags based on memory value's bits 7 and 6
  cpu.N = ((value and 0x80'u8) != 0) # Bit 7
  cpu.V = ((value and 0x40'u8) != 0) # Bit 6

  # 3. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 3 # BIT ZeroPage takes 3 cycles

 
proc opPHP*(cpu: var CPU) =
  ## PHP Implied - Opcode 0x08
  ## Action: Push Processor Status onto Stack (with bits 4 & 5 set)
  cpu.printOpCode("PHP")

  # Get current status and set bits 4 (Break) and 5 (Unused) for the pushed value
  let currentStatus = cpu.flags()
  let statusToPush = currentStatus or 0x30'u8 # Set bits 4 and 5

  # Push the modified status onto the stack
  cpu.push(statusToPush)

  # Update PC and Cycles
  cpu.PC += 1 # Implied addressing, 1 byte instruction
  cpu.cycles += 3 # PHP takes 3 cycles
 

proc opORA_imm*(cpu: var CPU) =
  ## ORA Immediate - Opcode 0x09
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, immediate)
  let value = result.value
  cpu.printOpCode(value, &"ORA #${value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 2 # ORA Immediate takes 2 cycles



proc opASL_acc*(cpu: var CPU) =
  ## ASL Accumulator - Opcode 0x0A
  ## Action: A = A << 1
  cpu.printOpCode("ASL A")

  # 1. Perform ASL on Accumulator
  let shiftedValue = cpu.performASL(cpu.A) # Updates C flag
  cpu.A = shiftedValue # Update Accumulator

  # 3. Update flags based on the *shifted* value in A
  cpu.updateZNFlags(cpu.A)

  # 4. Update PC and Cycles
  cpu.PC += 1 # Implied addressing, 1 byte instruction
  cpu.cycles += 2 # ASL Accumulator takes 2 cycles




proc opANC_imm*(cpu: var CPU) =
  ## ANC Immediate - Opcode 0x0B (unofficial)
  ## Action: A = A & M; C = N
  let result = resolveAddressingMode(cpu, immediate)
  let value = result.value
  cpu.printOpCode(value, &"ANC #${value.toHex:02}")

  # 1. Perform AND operation
  cpu.A = cpu.A and value

  # 2. Update N and Z flags based on the result in A
  cpu.updateZNFlags(cpu.A)

  # 3. Set Carry flag equal to the Negative flag
  cpu.C = cpu.N

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 2 # ANC Immediate takes 2 cycles




proc opORA_abs*(cpu: var CPU) =
  ## ORA Absolute - Opcode 0x0D
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, absolute)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA ${result.address.toHex:04} = {value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes)
  cpu.cycles += 4 # ORA Absolute takes 4 cycles



proc opNOP_abs*(cpu: var CPU) =
  ## NOP Absolute - Opcode 0x0C (unofficial)
  ## Action: Fetches operand address, reads from address, discards value.
  let result = resolveAddressingMode(cpu, absolute)
  # The read from the effective address happens implicitly within resolveAddressingMode
  # for absolute addressing mode when it fetches the address bytes.
  # An additional dummy read cycle might be needed depending on exact hardware behavior,
  # but the core absolute addressing fetch reads the target location.
  # For now, we assume resolveAddressingMode handles the necessary memory access cycle.
  cpu.printOpCode(result.address, &"NOP ${result.address.toHex:04}")

  # No operation performed with the value read
  # No flags or registers are affected

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  cpu.cycles += 4 # NOP abs takes 4 cycles




proc opNOP_absX*(cpu: var CPU) =
  ## NOP Absolute,X - Opcode 0x1C (unofficial)
  ## Action: Fetches operand address + X, reads from address, discards value.
  let result = resolveAddressingMode(cpu, absoluteX)
  # The read from the effective address happens implicitly within resolveAddressingMode
  # when calculating the final address.
  cpu.printOpCode(result.address, &"NOP ${result.address.toHex:04},X")

  # No operation performed with the value read
  # No flags or registers are affected

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  cpu.cycles += 4 + uint16(result.extraCycles) # NOP abs,X takes 4 cycles + 1 if page crossed

 

proc opSLO_zp*(cpu: var CPU) =
  ## SLO ZeroPage - Opcode 0x07 (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, zeroPage)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"SLO ${effectiveAddr.toHex:02} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 5 # SLO ZeroPage takes 5 cycles



proc opLDX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, immediate)
  cpu.printOpCode(result.value, &"LDX ${result.value.toHex:02}")
  cpu.X = result.value
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 2  # LDX immediate takes 2 cycles
  cpu.updateZNFlags(cpu.X)

proc opLDY_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDY ${result.address.toHex:02}")
  cpu.Y = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDY zp takes 3 cycles
  cpu.updateZNFlags(cpu.Y)

proc opLDA_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDA ${result.address.toHex:02}")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDA zp takes 3 cycles
  cpu.updateZNFlags(cpu.A)

proc opLDX_zp(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, zeroPage)
  cpu.printOpCode(result.address, &"LDX ${result.address.toHex:02}")
  cpu.X = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 3  # LDX zp takes 3 cycles
  cpu.updateZNFlags(cpu.X)

proc opLDA_imm(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, immediate)
  cpu.printOpCode(result.value, &"LDA ${result.value.toHex:02}")
  cpu.A = result.value
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 2  # LDA immediate takes 2 cycles
  cpu.updateZNFlags(cpu.A)

proc opLDA_indirectY(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, indirectY)
  cpu.printOpCode(result.address, &"LDA ({result.address.toHex:04}), Y")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 5 + uint16(result.extraCycles)  # LDA (zp),Y takes 5 cycles + 1 if page crossed
  cpu.updateZNFlags(cpu.A)

proc opLDA_absX(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, absoluteX)
  cpu.printOpCode(result.address, &"LDA ${result.address.toHex:04}, X")
  cpu.A = cpu.memory[result.address]
  cpu.PC += uint16(result.operandBytes + 1)
  cpu.cycles += 4 + uint16(result.extraCycles)  # LDA abs,X takes 4 cycles + 1 if page crossed
  cpu.updateZNFlags(cpu.A)

proc opBNE(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, relative)
  cpu.printOpCode(result.address, &"BNE ${result.address.toHex:02}")
  let baseCycles = 2
  cpu.cycles += uint16(baseCycles)
  cpu.performBranch(not cpu.Z, result)
  # PC increment only if branch not taken
  if cpu.Z:
    cpu.PC += uint16(result.operandBytes + 1)

proc opINX(cpu: var CPU) =
  cpu.printOpCode("INX")
  cpu.X += 1
  cpu.PC += 1
  cpu.cycles += 2  # INX takes 2 cycles
  cpu.updateZNFlags(cpu.X)

proc opBEQ(cpu: var CPU) =
  let result = resolveAddressingMode(cpu, relative)
  cpu.printOpCode(result.address, &"BEQ ${result.address.toHex:02}")
  let baseCycles = 2
  cpu.cycles += uint16(baseCycles)
  cpu.performBranch(cpu.Z, result)
  # PC increment only if branch not taken
  if not cpu.Z:
    cpu.PC += uint16(result.operandBytes + 1)

proc opKIL(cpu: var CPU) =
  ## KIL Implied - Opcode 0x02 (unofficial)
  ## Halts the processor.
  cpu.printOpCode("KIL")
  cpu.halted = true
  cpu.PC += 1 # KIL is a 1-byte instruction
  cpu.cycles += 2 # Nominal cycle count for KIL


proc opASL_abs*(cpu: var CPU) =
  ## ASL Absolute - Opcode 0x0E
  ## Action: M = M << 1
  let result = resolveAddressingMode(cpu, absolute)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"ASL ${effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  cpu.performASLOnMemory(effectiveAddr) # Handles shift, write back, C, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  cpu.cycles += 6 # ASL Absolute takes 6 cycles




proc opSLO_abs*(cpu: var CPU) =
  ## SLO Absolute - Opcode 0x0F (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, absolute)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"SLO ${effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  cpu.cycles += 6 # SLO Absolute takes 6 cycles




proc opSLO_indirectY*(cpu: var CPU) =
  ## SLO (Indirect),Y - Opcode 0x13 (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, indirectY) # Fetch address first
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"SLO (${(cpu.memory[cpu.PC + 1]).toHex:02}),Y @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")
  
  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue
  
  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags
  
  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  # Note: SLO (Indirect),Y always takes 8 cycles, regardless of page cross.
  # The extraCycles from resolveAddressingMode are ignored for this specific opcode.
  cpu.cycles += 8


proc opSLO_zpX*(cpu: var CPU) =
  ## SLO ZeroPage,X - Opcode 0x17 (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, zeroPageX) # Fetch address first
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"SLO (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {effectiveAddr.toHex:02} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 6 # SLO ZeroPage,X takes 6 cycles

  


proc opSLO_absY*(cpu: var CPU) =
  ## SLO Absolute,Y - Opcode 0x1B (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, absoluteY) # Fetch address first
  let effectiveAddr = result.address
  let baseAddr = uint16(cpu.memory[cpu.PC + 1]) or (uint16(cpu.memory[cpu.PC + 2]) shl 8) # For printing
  cpu.printOpCode(effectiveAddr, &"SLO ${baseAddr.toHex:04},Y @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes)
  # Note: SLO Absolute,Y always takes 7 cycles, regardless of page cross.
  # The extraCycles from resolveAddressingMode are ignored for this specific opcode.
  cpu.cycles += 7



proc opSLO_absX*(cpu: var CPU) =
  ## SLO Absolute,X - Opcode 0x1F (unofficial)
  ## Action: M = M << 1; A = A | M
  let result = resolveAddressingMode(cpu, absoluteX) # Use absoluteX
  let effectiveAddr = result.address
  let baseAddr = uint16(cpu.memory[cpu.PC + 1]) or (uint16(cpu.memory[cpu.PC + 2]) shl 8) # For printing
  cpu.printOpCode(effectiveAddr, &"SLO ${baseAddr.toHex:04},X @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  # 1. ASL on M
  let originalValue = cpu.memory[effectiveAddr]
  let shiftedValue = cpu.performASL(originalValue) # Updates C flag
  cpu.memory[effectiveAddr] = shiftedValue

  # 3. ORA with Accumulator using the *shifted* value
  cpu.performORA(shiftedValue) # Updates A, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  # Note: SLO Absolute,X always takes 7 cycles, regardless of page cross.
  # The extraCycles from resolveAddressingMode are ignored for this specific opcode.
  cpu.cycles += 7


proc opASL_zpX*(cpu: var CPU) =
  ## ASL ZeroPage,X - Opcode 0x16
  ## Action: M = M << 1
  let result = resolveAddressingMode(cpu, zeroPageX)
  let effectiveAddr = result.address
  cpu.printOpCode(effectiveAddr, &"ASL ${(cpu.memory[cpu.PC + 1]).toHex:02},X @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  cpu.performASLOnMemory(effectiveAddr) # Handles shift, write back, C, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 6 # ASL ZeroPage,X takes 6 cycles


proc opNOP_zpX*(cpu: var CPU) =
  ## NOP ZeroPage,X - Opcode 0x14 (unofficial)
  ## Action: Fetches operand, calculates address (operand + X), reads from address, discards value.
  let result = resolveAddressingMode(cpu, zeroPageX)
  # The read happens implicitly within resolveAddressingMode for zpX
  cpu.printOpCode(result.address, &"NOP ${(cpu.memory[cpu.PC + 1]).toHex:02},X @ {result.address.toHex:04}")

  # No operation performed with the value read
  # No flags or registers are affected

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 4 # NOP zp,X takes 4 cycles



proc opORA_zpX*(cpu: var CPU) =
  ## ORA ZeroPage,X - Opcode 0x15
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, zeroPageX)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA ${(cpu.memory[cpu.PC + 1]).toHex:02},X @ {result.address.toHex:02} = {value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 4 # ORA ZeroPage,X takes 4 cycles



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




proc opBPL*(cpu: var CPU) =
  ## BPL Relative - Opcode 0x10
  ## Branch if Negative flag is clear (N=0)
  let result = resolveAddressingMode(cpu, relative)
  # Note: resolveAddressingMode for relative already calculates the target address
  # and determines if a page cross occurs in result.extraCycles.
  cpu.printOpCode(result.address, &"BPL ${result.address.toHex:04}") # Use 04 for address consistency

  let baseCycles = 2
  cpu.cycles += uint16(baseCycles)
  cpu.performBranch(not cpu.N, result)
  # PC increment only if branch not taken
  if cpu.N:
    cpu.PC += uint16(result.operandBytes + 1)



proc opORA_indirectY*(cpu: var CPU) =
  ## ORA (Indirect),Y - Opcode 0x11
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, indirectY)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"ORA (${(cpu.memory[cpu.PC + 1]).toHex:02}),Y @ {result.address.toHex:04} = {value.toHex:02}")

  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 5 + uint16(result.extraCycles) # ORA (Indirect),Y takes 5 cycles (+1 if page crossed)



proc opORA_absY*(cpu: var CPU) =
  ## ORA Absolute,Y - Opcode 0x19
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, absoluteY)
  let value = cpu.memory[result.address]
  let baseAddr = uint16(cpu.memory[cpu.PC + 1]) or (uint16(cpu.memory[cpu.PC + 2]) shl 8)
  cpu.printOpCode(result.address, &"ORA ${baseAddr.toHex:04},Y @ {result.address.toHex:04} = {value.toHex:02}")
  
  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes)
  cpu.cycles += 4 + uint16(result.extraCycles) # ORA Absolute,Y takes 4 cycles (+1 if page crossed)




proc opORA_absX*(cpu: var CPU) =
  ## ORA Absolute,X - Opcode 0x1D
  ## Action: A = A | M
  let result = resolveAddressingMode(cpu, absoluteX)
  let value = cpu.memory[result.address]
  let baseAddr = uint16(cpu.memory[cpu.PC + 1]) or (uint16(cpu.memory[cpu.PC + 2]) shl 8)
  cpu.printOpCode(result.address, &"ORA ${baseAddr.toHex:04},X @ {result.address.toHex:04} = {value.toHex:02}")
  
  cpu.performORA(value)

  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes)
  cpu.cycles += 4 + uint16(result.extraCycles) # ORA Absolute,X takes 4 cycles (+1 if page crossed)



proc opASL_absX*(cpu: var CPU) =
  ## ASL Absolute,X - Opcode 0x1E
  ## Action: M = M << 1
  let result = resolveAddressingMode(cpu, absoluteX)
  let effectiveAddr = result.address
  let baseAddr = uint16(cpu.memory[cpu.PC + 1]) or (uint16(cpu.memory[cpu.PC + 2]) shl 8)
  cpu.printOpCode(effectiveAddr, &"ASL ${baseAddr.toHex:04},X @ {effectiveAddr.toHex:04} = {cpu.memory[effectiveAddr].toHex:02}")

  cpu.performASLOnMemory(effectiveAddr) # Handles shift, write back, C, Z, N flags

  # 4. Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + 2 operand bytes = 3)
  cpu.cycles += 7 # ASL Absolute,X takes 7 cycles (fixed)




proc opAND_21*(cpu: var CPU) =
  ## AND (Indirect,X) - Opcode 0x21
  ## Action: A = A & M
  let result = resolveAddressingMode(cpu, indirectX)
  let value = cpu.memory[result.address]
  cpu.printOpCode(result.address, &"AND (${(cpu.memory[cpu.PC + 1]).toHex:02},X) @ {result.address.toHex:04} = {value.toHex:02}")

  # Perform AND operation
  cpu.A = cpu.A and value

  # Update Z and N flags
  cpu.updateZNFlags(cpu.A)

  # Update PC and Cycles
  cpu.PC += uint16(result.operandBytes + 1) # Advance PC (opcode + operand byte)
  cpu.cycles += 6 + uint16(result.extraCycles) # AND (Indirect,X) takes 6 cycles


proc opCLC*(cpu: var CPU) =
  ## CLC Implied - Opcode 0x18
  ## Action: Clear Carry Flag (C = 0)
  cpu.printOpCode("CLC")

  # Clear the Carry flag
  cpu.C = false

  # Update PC and Cycles
  cpu.PC += 1 # Implied addressing, 1 byte instruction
  cpu.cycles += 2 # CLC takes 2 cycles




proc opNOP_implied_1A*(cpu: var CPU) =
  ## NOP Implied - Opcode 0x1A (unofficial)
  ## Action: No operation.
  cpu.printOpCode("NOP (1A)")

  # No flags or registers are affected

  cpu.PC += 1 # Advance PC (1 byte instruction)
  cpu.cycles += 2 # NOP implied takes 2 cycles


proc setupOpcodeTable*() =
  for i in 0..255:
    opcodeTable[i].handler = nil
    opcodeTable[i].cycles = 0
    opcodeTable[i].mode = immediate
    opcodeTable[i].mnemonic = "???"

  # Set up known opcodes
  opcodeTable[0x00] = OpcodeInfo(handler: opBRK, mode: immediate, cycles: 7, mnemonic: "BRK")
  opcodeTable[0x01] = OpcodeInfo(handler: opORA_indirectX, mode: indirectX, cycles: 6, mnemonic: "ORA")
  opcodeTable[0x02] = OpcodeInfo(handler: opKIL, mode: immediate, cycles: 2, mnemonic: "KIL") # Unofficial
  opcodeTable[0x03] = OpcodeInfo(handler: opSLO_indirectX, mode: indirectX, cycles: 8, mnemonic: "SLO") # Unofficial
  opcodeTable[0x04] = OpcodeInfo(handler: opNOP_zp, mode: zeroPage, cycles: 3, mnemonic: "NOP") # Unofficial
  opcodeTable[0x05] = OpcodeInfo(handler: opORA_zp, mode: zeroPage, cycles: 3, mnemonic: "ORA")
  opcodeTable[0x06] = OpcodeInfo(handler: opASL_zp, mode: zeroPage, cycles: 5, mnemonic: "ASL")
  opcodeTable[0x07] = OpcodeInfo(handler: opSLO_zp, mode: zeroPage, cycles: 5, mnemonic: "SLO") # Unofficial
  opcodeTable[0x21] = OpcodeInfo(handler: opAND_21, mode: indirectX, cycles: 6, mnemonic: "AND")
  opcodeTable[0x24] = OpcodeInfo(handler: opBIT_24, mode: zeroPage, cycles: 3, mnemonic: "BIT")

  opcodeTable[0x08] = OpcodeInfo(handler: opPHP, mode: immediate, cycles: 3, mnemonic: "PHP") # Technically implied, using immediate for consistency
  opcodeTable[0x09] = OpcodeInfo(handler: opORA_imm, mode: immediate, cycles: 2, mnemonic: "ORA")
  opcodeTable[0x0A] = OpcodeInfo(handler: opASL_acc, mode: immediate, cycles: 2, mnemonic: "ASL") # Accumulator mode
  opcodeTable[0x0B] = OpcodeInfo(handler: opANC_imm, mode: immediate, cycles: 2, mnemonic: "ANC") # Unofficial
  opcodeTable[0x0C] = OpcodeInfo(handler: opNOP_abs, mode: absolute, cycles: 4, mnemonic: "NOP") # Unofficial
  opcodeTable[0x0D] = OpcodeInfo(handler: opORA_abs, mode: absolute, cycles: 4, mnemonic: "ORA")
  opcodeTable[0x0E] = OpcodeInfo(handler: opASL_abs, mode: absolute, cycles: 6, mnemonic: "ASL")
  opcodeTable[0x0F] = OpcodeInfo(handler: opSLO_abs, mode: absolute, cycles: 6, mnemonic: "SLO") # Unofficial
  opcodeTable[0x10] = OpcodeInfo(handler: opBPL, mode: relative, cycles: 2, mnemonic: "BPL") # Cycles=2 base, +1 if branch taken, +1 more if page crossed
  opcodeTable[0x11] = OpcodeInfo(handler: opORA_indirectY, mode: indirectY, cycles: 5, mnemonic: "ORA") # Cycles=5 base, +1 if page crossed
  opcodeTable[0x12] = OpcodeInfo(handler: opKIL, mode: immediate, cycles: 2, mnemonic: "KIL") # Unofficial (Use opKIL)
  opcodeTable[0x13] = OpcodeInfo(handler: opSLO_indirectY, mode: indirectY, cycles: 8, mnemonic: "SLO") # Unofficial
  opcodeTable[0x14] = OpcodeInfo(handler: opNOP_zpX, mode: zeroPageX, cycles: 4, mnemonic: "NOP") # Unofficial
  opcodeTable[0x15] = OpcodeInfo(handler: opORA_zpX, mode: zeroPageX, cycles: 4, mnemonic: "ORA")
  opcodeTable[0x16] = OpcodeInfo(handler: opASL_zpX, mode: zeroPageX, cycles: 6, mnemonic: "ASL")
  opcodeTable[0x17] = OpcodeInfo(handler: opSLO_zpX, mode: zeroPageX, cycles: 6, mnemonic: "SLO") # Unofficial

  opcodeTable[0x18] = OpcodeInfo(handler: opCLC, mode: immediate, cycles: 2, mnemonic: "CLC") # Implied mode
  opcodeTable[0x20] = OpcodeInfo(handler: opJSR, mode: absolute, cycles: 6, mnemonic: "JSR")
  opcodeTable[0x60] = OpcodeInfo(handler: opRTS, mode: immediate, cycles: 6, mnemonic: "RTS")
  opcodeTable[0x84] = OpcodeInfo(handler: opSTY, mode: zeroPage, cycles: 3, mnemonic: "STY")
  opcodeTable[0x85] = OpcodeInfo(handler: opSTA, mode: zeroPage, cycles: 3, mnemonic: "STA")
  opcodeTable[0x86] = OpcodeInfo(handler: opSTX, mode: zeroPage, cycles: 3, mnemonic: "STX")
  opcodeTable[0xa0] = OpcodeInfo(handler: opLDY, mode: immediate, cycles: 2, mnemonic: "LDY")
  opcodeTable[0xa1] = OpcodeInfo(handler: opLDA_indirectX, mode: indirectX, cycles: 6, mnemonic: "LDA")
  opcodeTable[0xa2] = OpcodeInfo(handler: opLDX, mode: immediate, cycles: 2, mnemonic: "LDX")
  opcodeTable[0x1A] = OpcodeInfo(handler: opNOP_implied_1A, mode: immediate, cycles: 2, mnemonic: "NOP") # Unofficial, Implied mode
  opcodeTable[0x1B] = OpcodeInfo(handler: opSLO_absY, mode: absoluteY, cycles: 7, mnemonic: "SLO") # Unofficial
  opcodeTable[0x1C] = OpcodeInfo(handler: opNOP_absX, mode: absoluteX, cycles: 4, mnemonic: "NOP") # Unofficial
  opcodeTable[0x1D] = OpcodeInfo(handler: opORA_absX, mode: absoluteX, cycles: 4, mnemonic: "ORA") # Cycles=4 base, +1 if page crossed
  opcodeTable[0x1E] = OpcodeInfo(handler: opASL_absX, mode: absoluteX, cycles: 7, mnemonic: "ASL")
  opcodeTable[0x1F] = OpcodeInfo(handler: opSLO_absX, mode: absoluteX, cycles: 7, mnemonic: "SLO") # Unofficial
  opcodeTable[0xa4] = OpcodeInfo(handler: opLDY_zp, mode: zeroPage, cycles: 3, mnemonic: "LDY")
  opcodeTable[0xa5] = OpcodeInfo(handler: opLDA_zp, mode: zeroPage, cycles: 3, mnemonic: "LDA")
  opcodeTable[0xa6] = OpcodeInfo(handler: opLDX_zp, mode: zeroPage, cycles: 3, mnemonic: "LDX")
  opcodeTable[0xa9] = OpcodeInfo(handler: opLDA_imm, mode: immediate, cycles: 2, mnemonic: "LDA")
  opcodeTable[0xb1] = OpcodeInfo(handler: opLDA_indirectY, mode: indirectY, cycles: 5, mnemonic: "LDA")
  opcodeTable[0xbd] = OpcodeInfo(handler: opLDA_absX, mode: absoluteX, cycles: 4, mnemonic: "LDA")
  opcodeTable[0xd0] = OpcodeInfo(handler: opBNE, mode: relative, cycles: 2, mnemonic: "BNE")
  opcodeTable[0xe8] = OpcodeInfo(handler: opINX, mode: immediate, cycles: 2, mnemonic: "INX")
  opcodeTable[0x19] = OpcodeInfo(handler: opORA_absY, mode: absoluteY, cycles: 4, mnemonic: "ORA") # Cycles=4 base, +1 if page crossed
  opcodeTable[0xf0] = OpcodeInfo(handler: opBEQ, mode: relative, cycles: 2, mnemonic: "BEQ") # Cycles=2 base, +1 if branch taken, +1 more if page crossed

