import types
import addressing
import utils # For read16
import strformat
import strutils
import stack
import memory
import flags

export types.CPU
export types.OperatorMode, types.OpcodeHandler, types.OpcodeInfo

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

proc fetchOperandValue(cpu: var CPU, info: OpcodeInfo, addrResult: AddressingResult): uint8 =
  ## Fetches the operand value based on the addressing mode.
  if info.mode == immediate:
    addrResult.value
  else:
    cpu.memory[addrResult.address]



proc handleBranch(cpu: var CPU, info: OpcodeInfo, result: AddressingResult, condition: bool) =
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

proc updatePCAndCycles(cpu: var CPU, info: OpcodeInfo, result: AddressingResult) =
  ## Updates the Program Counter and CPU cycles based on opcode info and addressing result.
  cpu.PC += uint16(result.operandBytes + 1)
  if info.fixedCycles:
    cpu.cycles += uint16(info.cycles)
  else:
    cpu.cycles += uint16(info.cycles + result.extraCycles)

# Generic Opcode Handlers 

proc opLSR*(cpu: var CPU) =
  ## Stub for LSR (Logical Shift Right) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode LSR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opORA(cpu: var CPU) =
  ## Generic ORA handler.
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to operate on
  let value = fetchOperandValue(cpu, info, result)

  # Perform ORA logic
  cpu.A = cpu.A or value

  # Update flags
  cpu.updateZNFlags(cpu.A)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opLDA(cpu: var CPU) =
  ## Generic LDA handler.
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to operate on
  let value = fetchOperandValue(cpu, info, result)

  # Perform LDA logic
  cpu.A = value

  # Update flags
  cpu.updateZNFlags(cpu.A)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opSLO(cpu: var CPU) =
  ## Generic SLO handler (unofficial).
  ## Action: M = M << 1; A = A | M
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Read original value
  let originalValue = cpu.memory[result.address]

  # Perform ASL (M = M << 1)
  let shiftedValue = cpu.performASL(originalValue)

  # Write shifted value back to memory
  cpu.memory[result.address] = shiftedValue

  # Perform ORA (A = A | M)
  cpu.A = cpu.A or shiftedValue

  # Update flags based on final Accumulator value
  cpu.updateZNFlags(cpu.A)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opADC*(cpu: var CPU) =
  ## Stub for ADC (Add with Carry) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ADC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opAND(cpu: var CPU) =
  ## Generic AND handler.
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to operate on
  let value = fetchOperandValue(cpu, info, result)

  # Perform AND logic
  cpu.A = cpu.A and value

  # Update flags
  cpu.updateZNFlags(cpu.A)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opASL(cpu: var CPU) =
  ## Generic ASL handler (Arithmetic Shift Left).
  ## Action: M = M << 1 or A = A << 1
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]

  if info.mode == accumulator:
    # Accumulator Mode
    let originalValue = cpu.A
    let shiftedValue = cpu.performASL(originalValue)
    cpu.A = shiftedValue
    cpu.updateZNFlags(shiftedValue)

    cpu.PC += 1 # Accumulator mode is 1 byte
    cpu.cycles += uint16(info.cycles) # Accumulator mode always has fixed cycles for ASL (2 cycles)

  else:
    # Memory Modes
    let result = resolveAddressingMode(cpu, info.mode)
    let originalValue = cpu.memory[result.address]
    let shiftedValue = cpu.performASL(originalValue)

    cpu.memory[result.address] = shiftedValue
    cpu.updateZNFlags(shiftedValue)

    cpu.updatePCAndCycles(info, result)


proc opBIT(cpu: var CPU) =
  ## Generic BIT handler (Bit Test).
  ## Action: A & M, set N, V, Z flags
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction] # Assumes mode is ZeroPage or Absolute

  let result = resolveAddressingMode(cpu, info.mode)
  let value = fetchOperandValue(cpu, info, result)

  # Perform AND for Z flag
  let andResult = cpu.A and value
  cpu.setZ(andResult)

  # Set N flag based on bit 7 of the operand
  cpu.setN(value)

  # Set V flag based on bit 6 of the operand
  cpu.V = (value and 0x40'u8) != 0

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)



proc opBEQ(cpu: var CPU) =
  ## Generic BEQ handler (Branch if Equal - Z flag set).
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction] # Assumes mode is Relative
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  cpu.handleBranch(info, result, cpu.Z)


proc opBMI*(cpu: var CPU) =
  ## Stub for BMI (Branch if Minus) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BMI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBNE(cpu: var CPU) =
  ## Generic BNE handler (Branch if Not Equal - Z flag clear).
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction] # Assumes mode is Relative
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  cpu.handleBranch(info, result, not cpu.Z)


proc opANC*(cpu: var CPU) =
  ## Generic ANC handler (unofficial).
  ## Action: A = A & Immediate; C = N
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction] # Assumes mode is Immediate

  # ANC only uses Immediate mode
  # We could add an assertion here: assert info.mode == immediate
  let result = resolveAddressingMode(cpu, immediate) # Force immediate mode resolution
  let value = result.value

  # Perform AND logic
  cpu.A = cpu.A and value

  # Update Z and N flags based on the result in A
  cpu.updateZNFlags(cpu.A)

  # Set Carry flag based on the N flag (bit 7 of the result)
  cpu.C = cpu.N

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opBVC*(cpu: var CPU) =
  ## Stub for BVC (Branch if Overflow Clear) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BVC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBVS*(cpu: var CPU) =
  ## Stub for BVS (Branch if Overflow Set) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BVS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err

proc opBPL*(cpu: var CPU) =
  ## Stub for BPL (Branch if Plus - N flag clear) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BPL ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err



proc opBRK(cpu: var CPU) =
  ## Generic BRK handler (Force Break).
  ## Action: Push PC+2, Push P (with B=1), Set I=1, PC = ($FFFE)
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (though it's always 0x00 for BRK)
  let info = opcodeTable[instruction] # Get info (mainly for cycles)

  # 1. Increment PC (BRK is 1 byte, but interrupt pushes PC+2)
  #    Incrementing by 1 here simulates fetching the 'padding' byte.
  #    The address pushed will be PC+1 (after this increment).
  let pcToPush = cpu.PC + 2 # Calculate return address before pushing



  # 2. Push PC+1 onto the stack (high byte, then low byte)
  push(cpu, uint8(pcToPush shr 8))
  push(cpu, uint8(pcToPush and 0xFF))

  # 3. Push Processor Status onto the stack with B flag set and U flag clear
  var statusToPush: uint8 = 0
  if cpu.N: statusToPush = statusToPush or 0x80 # Bit 7
  if cpu.V: statusToPush = statusToPush or 0x40 # Bit 6
  # Bit 5 (Unused) is typically set when pushed
  statusToPush = statusToPush or 0x20
  # Bit 4 (Break) is set when pushed by BRK/PHP
  statusToPush = statusToPush or 0x10
  if cpu.D: statusToPush = statusToPush or 0x08 # Bit 3
  if cpu.I: statusToPush = statusToPush or 0x04 # Bit 2
  if cpu.Z: statusToPush = statusToPush or 0x02 # Bit 1
  if cpu.C: statusToPush = statusToPush or 0x01 # Bit 0
  push(cpu, statusToPush)

  # 4. Set Interrupt Disable flag (I flag) in CPU status
  cpu.I = true

  # 5. Load new PC from IRQ vector address ($FFFE/$FFFF)
  #    Using literals as requested, assuming IRQ_VECTOR_LOW/HIGH might not be defined/imported.
  let lowByte = cpu.memory[0xFFFE'u16]
  let highByte = cpu.memory[0xFFFF'u16]
  cpu.PC = (uint16(highByte) shl 8) or uint16(lowByte)

  # 6. Update CPU cycles (BRK always takes 7 cycles)
  cpu.cycles += uint16(info.cycles) # Use cycles from table (should be 7)


proc opCLC(cpu: var CPU) =
  ## Generic CLC handler (Clear Carry Flag).
  ## Action: C = 0
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (though it's always 0x18 for CLC)
  let info = opcodeTable[instruction] # Get info (mainly for cycles)

  # Perform CLC logic
  cpu.C = false

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, AddressingResult(operandBytes: 0, extraCycles: 0)) # Implied mode has 0 operand bytes and 0 extra cycles

proc opCLI*(cpu: var CPU) =
  ## Stub for CLI (Clear Interrupt Disable) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CLI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err



  # Update CPU cycles (CLC always takes 2 cycles)


proc opEOR*(cpu: var CPU) =
  ## Stub for EOR (Exclusive OR) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode EOR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opINX(cpu: var CPU) =
  ## Generic INX handler (Increment X Register).
  ## Action: X = X + 1
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (e.g., 0xE8)
  let info = opcodeTable[instruction] # Get info (mainly for cycles)

  # Perform INX logic
  cpu.X = cpu.X + 1 # Increment X with wraparound (standard '+' wraps for uint8)

  # Update flags
  cpu.updateZNFlags(cpu.X) # Set Z and N based on the new value of X

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, AddressingResult(operandBytes: 0, extraCycles: 0)) # Implied mode has 0 operand bytes and 0 extra cycles


proc opJMP*(cpu: var CPU) =
  ## Stub for JMP (Jump) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode JMP ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opJSR(cpu: var CPU) =
  ## Generic JSR handler (Jump to Subroutine).
  ## Action: Push PC+2, PC = Address
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction] # Get info (mode=Absolute, cycles=6)
  let result = resolveAddressingMode(cpu, info.mode) # Resolve Absolute address

  # Calculate the return address (address of the instruction *after* JSR)
  # JSR is 3 bytes (opcode + 2 address bytes), so return is PC + 2
  let returnAddress = cpu.PC + 2

  # Push the return address onto the stack (high byte, then low byte)
  push(cpu, uint8(returnAddress shr 8))
  push(cpu, uint8(returnAddress and 0xFF))

  # Set the Program Counter to the target subroutine address
  cpu.PC = result.address

  # Update CPU cycles (JSR Absolute always takes 6 cycles)
  cpu.cycles += uint16(info.cycles) # Use cycles from table (should be 6)




proc opKIL*(cpu: var CPU) =
  ## Generic KIL handler (unofficial).
  ## Action: Halt the CPU.
  cpu.halted = true
  # No PC or cycle update needed as the CPU stops.


proc opLDX(cpu: var CPU) =
  ## Generic LDX handler (Load X Register).
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to load
  let value = fetchOperandValue(cpu, info, result)

  # Perform LDX logic
  cpu.X = value

  # Update flags
  cpu.updateZNFlags(cpu.X)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opLDY(cpu: var CPU) =
  ## Generic LDY handler (Load Y Register).
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to load
  let value = fetchOperandValue(cpu, info, result)

  # Perform LDY logic
  cpu.Y = value

  # Update flags
  cpu.updateZNFlags(cpu.Y)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opNOP*(cpu: var CPU) =
  ## Generic NOP handler (No Operation).
  ## Action: Does nothing except advance PC and consume cycles.
  ## Handles official and unofficial NOPs with varying lengths and cycle counts.
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]

  # Resolve addressing mode to determine operand bytes and potential extra cycles,
  # even if the operand value/address isn't used. Some unofficial NOPs
  # perform memory reads.
  let result = resolveAddressingMode(cpu, info.mode)

  # NOP performs no action on registers or flags.

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opPLP*(cpu: var CPU) =
  ## Stub for PLP (Pull Processor Status from Stack) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode PLP ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opPLA*(cpu: var CPU) =
  ## Stub for PLA (Pull Accumulator) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode PLA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opPHA*(cpu: var CPU) =
  ## Stub for PHA (Push Accumulator) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode PHA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opPHP(cpu: var CPU) =
  ## Generic PHP handler (Push Processor Status on Stack).
  ## Action: Push P (with B=1, U=1)
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (e.g., 0x08)
  let info = opcodeTable[instruction] # Get info (mode=Implied, cycles=3)

  # Get current status byte
  var statusToPush: uint8 = 0
  if cpu.N: statusToPush = statusToPush or 0x80 # Bit 7
  if cpu.V: statusToPush = statusToPush or 0x40 # Bit 6
  # Bit 5 (Unused) is set when pushed by PHP/BRK
  statusToPush = statusToPush or 0x20
  # Bit 4 (Break) is set when pushed by PHP/BRK
  statusToPush = statusToPush or 0x10
  if cpu.D: statusToPush = statusToPush or 0x08 # Bit 3
  if cpu.I: statusToPush = statusToPush or 0x04 # Bit 2
  if cpu.Z: statusToPush = statusToPush or 0x02 # Bit 1
  if cpu.C: statusToPush = statusToPush or 0x01 # Bit 0

  # Push the modified status byte onto the stack
  push(cpu, statusToPush)

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, AddressingResult(operandBytes: 0, extraCycles: 0)) # Implied mode has 0 operand bytes and 0 extra cycles


proc opROL*(cpu: var CPU) =
  ## Stub for ROL (Rotate Left) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ROL ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opROR*(cpu: var CPU) =
  ## Stub for ROR (Rotate Right) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ROR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opRTS(cpu: var CPU) =
  ## Generic RTS handler (Return from Subroutine).
  ## Action: Pull PC from stack, PC = Pulled PC + 1
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (always 0x60 for RTS)
  let info = opcodeTable[instruction] # Get info (mode=Implied, cycles=6)

  # Pull the return address (minus 1) from the stack
  # Stack order: high byte pushed first by JSR, so pull low byte first
  let lowByte = pull(cpu)
  let highByte = pull(cpu)
  let returnAddress = (uint16(highByte) shl 8) or uint16(lowByte)

  # Set the Program Counter to the address *after* the JSR instruction
  cpu.PC = returnAddress + 1

  # Update CPU cycles (RTS always takes 6 cycles)
  cpu.cycles += uint16(info.cycles) # Use cycles from table (should be 6)


proc opRTI*(cpu: var CPU) =
  ## Stub for RTI (Return from Interrupt) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode RTI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSEC*(cpu: var CPU) =
  ## Stub for SEC (Set Carry Flag) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SEC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSEI*(cpu: var CPU) =
  ## Stub for SEI (Set Interrupt Disable) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SEI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSTA(cpu: var CPU) =
  ## Generic STA handler (Store Accumulator in Memory).
  ## Action: M = A
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  # STA does not use Immediate mode, only memory addressing modes.
  let result = resolveAddressingMode(cpu, info.mode)

  # Perform STA logic: Write Accumulator to the resolved address
  cpu.memory[result.address] = cpu.A

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opSTX(cpu: var CPU) =
  ## Generic STX handler (Store X Register in Memory).
  ## Action: M = X
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  # STX does not use Immediate mode, only memory addressing modes.
  let result = resolveAddressingMode(cpu, info.mode)

  # Perform STX logic: Write X register to the resolved address
  cpu.memory[result.address] = cpu.X

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


proc opSTY(cpu: var CPU) =
  ## Generic STY handler (Store Y Register in Memory).
  ## Action: M = Y
  let instruction = cpu.memory[cpu.PC]
  let info = opcodeTable[instruction]
  # STY uses ZeroPage, ZeroPageX, Absolute modes.
  let result = resolveAddressingMode(cpu, info.mode)

  # Perform STY logic: Write Y register to the resolved address
  cpu.memory[result.address] = cpu.Y

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, result)


# --- Placeholder for other generic handlers ---
# opADC, opSBC, opCMP, opCPX, opCPY, opINC, opDEC, opROL, opLSR, opROR,
# opJMP, opBCC, opBCS, opBVC, opBVS, opBMI, opPLP, opPHA, opPLA,
# opDEY, opTAY, opINY, opSEC, opCLI, opSEI, opTYA, opCLV, opCLD, opSED,
# opTXA, opTAX, opTXS, opTSX, opRTI,
# opRLA, opSRE, opRRA, opAXS, opLAX, opDCP, opISC, opALR, opARR,
# opXAA, opTAS, opSHY, opSHX, opLAS
# --- End Placeholder ---

# --- Start of Old Specific Handlers (To be removed in Step 5) ---
# proc opORA_indirectX*(cpu: var CPU) = ...
# proc opSLO_indirectX*(cpu: var CPU) = ...
# proc opORA_zp*(cpu: var CPU) = ...
# proc opASL_zp*(cpu: var CPU) = ...
# proc opBIT_24*(cpu: var CPU) = ...
# proc opAND_25*(cpu: var CPU) = ...
# proc opPHP*(cpu: var CPU) = ...
# proc opORA_imm*(cpu: var CPU) = ...
# proc opASL_acc*(cpu: var CPU) = ...
# proc opANC_imm*(cpu: var CPU) = ...
# proc opORA_abs*(cpu: var CPU) = ...
# proc opNOP_abs*(cpu: var CPU) = ...
# proc opNOP_absX*(cpu: var CPU) = ...
# proc opSLO_zp*(cpu: var CPU) = ...
# proc opLDX(cpu: var CPU) = ... # Example, might be generic already
# proc opLDY_zp(cpu: var CPU) = ...
# proc opLDA_zp(cpu: var CPU) = ...
# proc opLDX_zp(cpu: var CPU) = ...
# proc opLDA_imm(cpu: var CPU) = ...
# proc opLDA_indirectY(cpu: var CPU) = ...
# proc opLDA_absX(cpu: var CPU) = ...
# proc opBNE(cpu: var CPU) = ... # Example, might be generic already
# proc opINX(cpu: var CPU) = ... # Example, might be generic already
# proc opBEQ(cpu: var CPU) = ... # Example, might be generic already
# proc opKIL(cpu: var CPU) = ... # Example, might be generic already
# proc opASL_abs*(cpu: var CPU) = ...
# proc opSLO_abs*(cpu: var CPU) = ...
# proc opSLO_indirectY*(cpu: var CPU) = ...
# proc opSLO_zpX*(cpu: var CPU) = ...
# proc opSLO_absY*(cpu: var CPU) = ...
# proc opSLO_absX*(cpu: var CPU) = ...
# proc opASL_zpX*(cpu: var CPU) = ...
# proc opNOP_zpX*(cpu: var CPU) = ...
# proc opORA_zpX*(cpu: var CPU) = ...
# proc opNOP_zp*(cpu: var CPU) = ...
# proc opBPL*(cpu: var CPU) = ... # Example, might be generic already
# proc opORA_indirectY*(cpu: var CPU) = ...
# proc opORA_absY*(cpu: var CPU) = ...
# proc opORA_absX*(cpu: var CPU) = ...
# proc opASL_absX*(cpu: var CPU) = ...
# proc opAND_21*(cpu: var CPU) = ...
# proc opCLC*(cpu: var CPU) = ... # Example, might be generic already
# proc opNOP_implied_1A*(cpu: var CPU) = ...
# --- End of Old Specific Handlers ---


proc opDEY*(cpu: var CPU) =
  ## Stub for DEY (Decrement Y Register) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode DEY ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTXA*(cpu: var CPU) =
  ## Stub for TXA (Transfer X to Accumulator) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TXA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBCC*(cpu: var CPU) =
  ## Stub for BCC (Branch if Carry Clear) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BCC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTYA*(cpu: var CPU) =
  ## Stub for TYA (Transfer Y to Accumulator) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TYA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTXS*(cpu: var CPU) =
  ## Stub for TXS (Transfer X to Stack Pointer) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TXS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTAY*(cpu: var CPU) =
  ## Stub for TAY (Transfer Accumulator to Y) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TAY ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTAX(cpu: var CPU) =
  ## Generic TAX handler (Transfer Accumulator to X).
  ## Action: X = A
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (e.g., 0xAA)
  let info = opcodeTable[instruction] # Get info (mainly for cycles)

  # Perform TAX logic
  cpu.X = cpu.A

  # Update flags
  cpu.updateZNFlags(cpu.X) # Set Z and N based on the new value of X

  # Update Program Counter and CPU cycles
  cpu.updatePCAndCycles(info, AddressingResult(operandBytes: 0, extraCycles: 0)) # Implied mode has 0 operand bytes and 0 extra cycles


proc opBCS*(cpu: var CPU) =
  ## Stub for BCS (Branch if Carry Set) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BCS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opCLV*(cpu: var CPU) =
  ## Stub for CLV (Clear Overflow Flag) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CLV ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTSX*(cpu: var CPU) =
  ## Stub for TSX (Transfer Stack Pointer to X) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TSX ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opCPY*(cpu: var CPU) =
  ## Stub for CPY (Compare Y Register) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CPY ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opCMP*(cpu: var CPU) =
  ## Stub for CMP (Compare Accumulator) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CMP ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opDEC*(cpu: var CPU) =
  ## Stub for DEC (Decrement Memory) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode DEC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opINY*(cpu: var CPU) =
  ## Stub for INY (Increment Y Register) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode INY ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opDEX*(cpu: var CPU) =
  ## Stub for DEX (Decrement X Register) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode DEX ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opCLD*(cpu: var CPU) =
  ## Stub for CLD (Clear Decimal Mode) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CLD ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opCPX*(cpu: var CPU) =
  ## Stub for CPX (Compare X Register) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode CPX ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSBC*(cpu: var CPU) =
  ## Stub for SBC (Subtract with Carry) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SBC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opINC*(cpu: var CPU) =
  ## Stub for INC (Increment Memory) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode INC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSED*(cpu: var CPU) =
  ## Stub for SED (Set Decimal Flag) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SED ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opRLA*(cpu: var CPU) =
  ## Stub for RLA (Rotate Left then AND) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode RLA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSRE*(cpu: var CPU) =
  ## Stub for SRE (Shift Right then EOR) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SRE ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opALR*(cpu: var CPU) =
  ## Stub for ALR (AND then LSR) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ALR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opRRA*(cpu: var CPU) =
  ## Stub for RRA (Rotate Right then Add with Carry) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode RRA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opARR*(cpu: var CPU) =
  ## Stub for ARR (AND then ROR) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ARR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opAXS*(cpu: var CPU) =
  ## Stub for AXS (AND X then SUB) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode AXS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opXAA*(cpu: var CPU) =
  ## Stub for XAA (Transfer X to A then AND) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode XAA ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opTAS*(cpu: var CPU) =
  ## Stub for TAS (SHA/AHX variant) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode TAS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSHY*(cpu: var CPU) =
  ## Stub for SHY (SYA) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SHY ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opSHX*(cpu: var CPU) =
  ## Stub for SHX (SXA) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode SHX ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opLAX*(cpu: var CPU) =
  ## Stub for LAX (Load A and X) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode LAX ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opLAS*(cpu: var CPU) =
  ## Stub for LAS (Load A, X, S from Memory AND SP) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode LAS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opDCP*(cpu: var CPU) =
  ## Stub for DCP (Decrement Memory then Compare) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode DCP ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opISC*(cpu: var CPU) =
  ## Stub for ISC (Increment Memory then Subtract with Carry) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode ISC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc setupOpcodeTable*() =
  ## Initializes the opcode lookup table with information for all 256 possible opcodes.
  ## Uses generic handlers and specifies fixed/variable cycle counts.

  # Default for invalid/unused opcodes (often treated as NOP or KIL)
  for i in 0..255:
    opcodeTable[i] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP") # Default to NOP implied

  # Official Opcodes
  opcodeTable[0x00] = OpcodeInfo(fixedCycles: true, handler: opBRK, cycles: 7, mode: implied, mnemonic: "BRK")
  opcodeTable[0x01] = OpcodeInfo(fixedCycles: true, handler: opORA, cycles: 6, mode: indirectX, mnemonic: "ORA")
  opcodeTable[0x05] = OpcodeInfo(fixedCycles: true, handler: opORA, cycles: 3, mode: zeroPage, mnemonic: "ORA")
  opcodeTable[0x06] = OpcodeInfo(fixedCycles: true, handler: opASL, cycles: 5, mode: zeroPage, mnemonic: "ASL")
  opcodeTable[0x08] = OpcodeInfo(fixedCycles: true, handler: opPHP, cycles: 3, mode: implied, mnemonic: "PHP")
  opcodeTable[0x09] = OpcodeInfo(fixedCycles: true, handler: opORA, cycles: 2, mode: immediate, mnemonic: "ORA")
  opcodeTable[0x0A] = OpcodeInfo(fixedCycles: true, handler: opASL, cycles: 2, mode: accumulator, mnemonic: "ASL")
  opcodeTable[0x0D] = OpcodeInfo(fixedCycles: true, handler: opORA, cycles: 4, mode: absolute, mnemonic: "ORA")
  opcodeTable[0x0E] = OpcodeInfo(fixedCycles: true, handler: opASL, cycles: 6, mode: absolute, mnemonic: "ASL")
  opcodeTable[0x10] = OpcodeInfo(fixedCycles: false, handler: opBPL, cycles: 2, mode: relative, mnemonic: "BPL") # 2++
  opcodeTable[0x11] = OpcodeInfo(fixedCycles: false, handler: opORA, cycles: 5, mode: indirectY, mnemonic: "ORA") # 5+
  opcodeTable[0x15] = OpcodeInfo(fixedCycles: true, handler: opORA, cycles: 4, mode: zeroPageX, mnemonic: "ORA")
  opcodeTable[0x16] = OpcodeInfo(fixedCycles: true, handler: opASL, cycles: 6, mode: zeroPageX, mnemonic: "ASL")
  opcodeTable[0x18] = OpcodeInfo(fixedCycles: true, handler: opCLC, cycles: 2, mode: implied, mnemonic: "CLC")
  opcodeTable[0x19] = OpcodeInfo(fixedCycles: false, handler: opORA, cycles: 4, mode: absoluteY, mnemonic: "ORA") # 4+
  opcodeTable[0x1D] = OpcodeInfo(fixedCycles: false, handler: opORA, cycles: 4, mode: absoluteX, mnemonic: "ORA") # 4+
  opcodeTable[0x1E] = OpcodeInfo(fixedCycles: true, handler: opASL, cycles: 7, mode: absoluteX, mnemonic: "ASL")
  opcodeTable[0x20] = OpcodeInfo(fixedCycles: true, handler: opJSR, cycles: 6, mode: absolute, mnemonic: "JSR")
  opcodeTable[0x21] = OpcodeInfo(fixedCycles: true, handler: opAND, cycles: 6, mode: indirectX, mnemonic: "AND")
  opcodeTable[0x24] = OpcodeInfo(fixedCycles: true, handler: opBIT, cycles: 3, mode: zeroPage, mnemonic: "BIT")
  opcodeTable[0x25] = OpcodeInfo(fixedCycles: true, handler: opAND, cycles: 3, mode: zeroPage, mnemonic: "AND")
  opcodeTable[0x26] = OpcodeInfo(fixedCycles: true, handler: opROL, cycles: 5, mode: zeroPage, mnemonic: "ROL")
  opcodeTable[0x28] = OpcodeInfo(fixedCycles: true, handler: opPLP, cycles: 4, mode: implied, mnemonic: "PLP")
  opcodeTable[0x29] = OpcodeInfo(fixedCycles: true, handler: opAND, cycles: 2, mode: immediate, mnemonic: "AND")
  opcodeTable[0x2A] = OpcodeInfo(fixedCycles: true, handler: opROL, cycles: 2, mode: accumulator, mnemonic: "ROL")
  opcodeTable[0x2C] = OpcodeInfo(fixedCycles: true, handler: opBIT, cycles: 4, mode: absolute, mnemonic: "BIT")
  opcodeTable[0x2D] = OpcodeInfo(fixedCycles: true, handler: opAND, cycles: 4, mode: absolute, mnemonic: "AND")
  opcodeTable[0x2E] = OpcodeInfo(fixedCycles: true, handler: opROL, cycles: 6, mode: absolute, mnemonic: "ROL")
  opcodeTable[0x30] = OpcodeInfo(fixedCycles: false, handler: opBMI, cycles: 2, mode: relative, mnemonic: "BMI") # 2++
  opcodeTable[0x31] = OpcodeInfo(fixedCycles: false, handler: opAND, cycles: 5, mode: indirectY, mnemonic: "AND") # 5+
  opcodeTable[0x35] = OpcodeInfo(fixedCycles: true, handler: opAND, cycles: 4, mode: zeroPageX, mnemonic: "AND")
  opcodeTable[0x36] = OpcodeInfo(fixedCycles: true, handler: opROL, cycles: 6, mode: zeroPageX, mnemonic: "ROL")
  opcodeTable[0x38] = OpcodeInfo(fixedCycles: true, handler: opSEC, cycles: 2, mode: implied, mnemonic: "SEC")
  opcodeTable[0x39] = OpcodeInfo(fixedCycles: false, handler: opAND, cycles: 4, mode: absoluteY, mnemonic: "AND") # 4+
  opcodeTable[0x3D] = OpcodeInfo(fixedCycles: false, handler: opAND, cycles: 4, mode: absoluteX, mnemonic: "AND") # 4+
  opcodeTable[0x3E] = OpcodeInfo(fixedCycles: true, handler: opROL, cycles: 7, mode: absoluteX, mnemonic: "ROL")
  opcodeTable[0x40] = OpcodeInfo(fixedCycles: true, handler: opRTI, cycles: 6, mode: implied, mnemonic: "RTI")
  opcodeTable[0x41] = OpcodeInfo(fixedCycles: true, handler: opEOR, cycles: 6, mode: indirectX, mnemonic: "EOR")
  opcodeTable[0x45] = OpcodeInfo(fixedCycles: true, handler: opEOR, cycles: 3, mode: zeroPage, mnemonic: "EOR")
  opcodeTable[0x46] = OpcodeInfo(fixedCycles: true, handler: opLSR, cycles: 5, mode: zeroPage, mnemonic: "LSR")
  opcodeTable[0x48] = OpcodeInfo(fixedCycles: true, handler: opPHA, cycles: 3, mode: implied, mnemonic: "PHA")
  opcodeTable[0x49] = OpcodeInfo(fixedCycles: true, handler: opEOR, cycles: 2, mode: immediate, mnemonic: "EOR")
  opcodeTable[0x4A] = OpcodeInfo(fixedCycles: true, handler: opLSR, cycles: 2, mode: accumulator, mnemonic: "LSR")
  opcodeTable[0x4C] = OpcodeInfo(fixedCycles: true, handler: opJMP, cycles: 3, mode: absolute, mnemonic: "JMP")
  opcodeTable[0x4D] = OpcodeInfo(fixedCycles: true, handler: opEOR, cycles: 4, mode: absolute, mnemonic: "EOR")
  opcodeTable[0x4E] = OpcodeInfo(fixedCycles: true, handler: opLSR, cycles: 6, mode: absolute, mnemonic: "LSR")
  opcodeTable[0x50] = OpcodeInfo(fixedCycles: false, handler: opBVC, cycles: 2, mode: relative, mnemonic: "BVC") # 2++
  opcodeTable[0x51] = OpcodeInfo(fixedCycles: false, handler: opEOR, cycles: 5, mode: indirectY, mnemonic: "EOR") # 5+
  opcodeTable[0x55] = OpcodeInfo(fixedCycles: true, handler: opEOR, cycles: 4, mode: zeroPageX, mnemonic: "EOR")
  opcodeTable[0x56] = OpcodeInfo(fixedCycles: true, handler: opLSR, cycles: 6, mode: zeroPageX, mnemonic: "LSR")
  opcodeTable[0x58] = OpcodeInfo(fixedCycles: true, handler: opCLI, cycles: 2, mode: implied, mnemonic: "CLI")
  opcodeTable[0x59] = OpcodeInfo(fixedCycles: false, handler: opEOR, cycles: 4, mode: absoluteY, mnemonic: "EOR") # 4+
  opcodeTable[0x5D] = OpcodeInfo(fixedCycles: false, handler: opEOR, cycles: 4, mode: absoluteX, mnemonic: "EOR") # 4+
  opcodeTable[0x5E] = OpcodeInfo(fixedCycles: true, handler: opLSR, cycles: 7, mode: absoluteX, mnemonic: "LSR")
  opcodeTable[0x60] = OpcodeInfo(fixedCycles: true, handler: opRTS, cycles: 6, mode: implied, mnemonic: "RTS")
  opcodeTable[0x61] = OpcodeInfo(fixedCycles: true, handler: opADC, cycles: 6, mode: indirectX, mnemonic: "ADC")
  opcodeTable[0x65] = OpcodeInfo(fixedCycles: true, handler: opADC, cycles: 3, mode: zeroPage, mnemonic: "ADC")
  opcodeTable[0x66] = OpcodeInfo(fixedCycles: true, handler: opROR, cycles: 5, mode: zeroPage, mnemonic: "ROR")
  opcodeTable[0x68] = OpcodeInfo(fixedCycles: true, handler: opPLA, cycles: 4, mode: implied, mnemonic: "PLA")
  opcodeTable[0x69] = OpcodeInfo(fixedCycles: true, handler: opADC, cycles: 2, mode: immediate, mnemonic: "ADC")
  opcodeTable[0x6A] = OpcodeInfo(fixedCycles: true, handler: opROR, cycles: 2, mode: accumulator, mnemonic: "ROR")
  opcodeTable[0x6C] = OpcodeInfo(fixedCycles: true, handler: opJMP, cycles: 5, mode: indirect, mnemonic: "JMP")
  opcodeTable[0x6D] = OpcodeInfo(fixedCycles: true, handler: opADC, cycles: 4, mode: absolute, mnemonic: "ADC")
  opcodeTable[0x6E] = OpcodeInfo(fixedCycles: true, handler: opROR, cycles: 6, mode: absolute, mnemonic: "ROR")
  opcodeTable[0x70] = OpcodeInfo(fixedCycles: false, handler: opBVS, cycles: 2, mode: relative, mnemonic: "BVS") # 2++
  opcodeTable[0x71] = OpcodeInfo(fixedCycles: false, handler: opADC, cycles: 5, mode: indirectY, mnemonic: "ADC") # 5+
  opcodeTable[0x75] = OpcodeInfo(fixedCycles: true, handler: opADC, cycles: 4, mode: zeroPageX, mnemonic: "ADC")
  opcodeTable[0x76] = OpcodeInfo(fixedCycles: true, handler: opROR, cycles: 6, mode: zeroPageX, mnemonic: "ROR")
  opcodeTable[0x78] = OpcodeInfo(fixedCycles: true, handler: opSEI, cycles: 2, mode: implied, mnemonic: "SEI")
  opcodeTable[0x79] = OpcodeInfo(fixedCycles: false, handler: opADC, cycles: 4, mode: absoluteY, mnemonic: "ADC") # 4+
  opcodeTable[0x7D] = OpcodeInfo(fixedCycles: false, handler: opADC, cycles: 4, mode: absoluteX, mnemonic: "ADC") # 4+
  opcodeTable[0x7E] = OpcodeInfo(fixedCycles: true, handler: opROR, cycles: 7, mode: absoluteX, mnemonic: "ROR")
  opcodeTable[0x81] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 6, mode: indirectX, mnemonic: "STA")
  opcodeTable[0x84] = OpcodeInfo(fixedCycles: true, handler: opSTY, cycles: 3, mode: zeroPage, mnemonic: "STY")
  opcodeTable[0x85] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 3, mode: zeroPage, mnemonic: "STA")
  opcodeTable[0x86] = OpcodeInfo(fixedCycles: true, handler: opSTX, cycles: 3, mode: zeroPage, mnemonic: "STX")
  opcodeTable[0x88] = OpcodeInfo(fixedCycles: true, handler: opDEY, cycles: 2, mode: implied, mnemonic: "DEY")
  opcodeTable[0x8A] = OpcodeInfo(fixedCycles: true, handler: opTXA, cycles: 2, mode: implied, mnemonic: "TXA")
  opcodeTable[0x8C] = OpcodeInfo(fixedCycles: true, handler: opSTY, cycles: 4, mode: absolute, mnemonic: "STY")
  opcodeTable[0x8D] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 4, mode: absolute, mnemonic: "STA")
  opcodeTable[0x8E] = OpcodeInfo(fixedCycles: true, handler: opSTX, cycles: 4, mode: absolute, mnemonic: "STX")
  opcodeTable[0x90] = OpcodeInfo(fixedCycles: false, handler: opBCC, cycles: 2, mode: relative, mnemonic: "BCC") # 2++
  opcodeTable[0x91] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 6, mode: indirectY, mnemonic: "STA")
  opcodeTable[0x94] = OpcodeInfo(fixedCycles: true, handler: opSTY, cycles: 4, mode: zeroPageX, mnemonic: "STY")
  opcodeTable[0x95] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 4, mode: zeroPageX, mnemonic: "STA")
  opcodeTable[0x96] = OpcodeInfo(fixedCycles: true, handler: opSTX, cycles: 4, mode: zeroPageY, mnemonic: "STX")
  opcodeTable[0x98] = OpcodeInfo(fixedCycles: true, handler: opTYA, cycles: 2, mode: implied, mnemonic: "TYA")
  opcodeTable[0x99] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 5, mode: absoluteY, mnemonic: "STA")
  opcodeTable[0x9A] = OpcodeInfo(fixedCycles: true, handler: opTXS, cycles: 2, mode: implied, mnemonic: "TXS")
  opcodeTable[0x9D] = OpcodeInfo(fixedCycles: true, handler: opSTA, cycles: 5, mode: absoluteX, mnemonic: "STA")
  opcodeTable[0xA0] = OpcodeInfo(fixedCycles: true, handler: opLDY, cycles: 2, mode: immediate, mnemonic: "LDY")
  opcodeTable[0xA1] = OpcodeInfo(fixedCycles: true, handler: opLDA, cycles: 6, mode: indirectX, mnemonic: "LDA")
  opcodeTable[0xA2] = OpcodeInfo(fixedCycles: true, handler: opLDX, cycles: 2, mode: immediate, mnemonic: "LDX")
  opcodeTable[0xA4] = OpcodeInfo(fixedCycles: true, handler: opLDY, cycles: 3, mode: zeroPage, mnemonic: "LDY")
  opcodeTable[0xA5] = OpcodeInfo(fixedCycles: true, handler: opLDA, cycles: 3, mode: zeroPage, mnemonic: "LDA")
  opcodeTable[0xA6] = OpcodeInfo(fixedCycles: true, handler: opLDX, cycles: 3, mode: zeroPage, mnemonic: "LDX")
  opcodeTable[0xA8] = OpcodeInfo(fixedCycles: true, handler: opTAY, cycles: 2, mode: implied, mnemonic: "TAY")
  opcodeTable[0xA9] = OpcodeInfo(fixedCycles: true, handler: opLDA, cycles: 2, mode: immediate, mnemonic: "LDA")
  opcodeTable[0xAA] = OpcodeInfo(fixedCycles: true, handler: opTAX, cycles: 2, mode: implied, mnemonic: "TAX")
  opcodeTable[0xAC] = OpcodeInfo(fixedCycles: true, handler: opLDY, cycles: 4, mode: absolute, mnemonic: "LDY")
  opcodeTable[0xAD] = OpcodeInfo(fixedCycles: true, handler: opLDA, cycles: 4, mode: absolute, mnemonic: "LDA")
  opcodeTable[0xAE] = OpcodeInfo(fixedCycles: true, handler: opLDX, cycles: 4, mode: absolute, mnemonic: "LDX")
  opcodeTable[0xB0] = OpcodeInfo(fixedCycles: false, handler: opBCS, cycles: 2, mode: relative, mnemonic: "BCS") # 2++
  opcodeTable[0xB1] = OpcodeInfo(fixedCycles: false, handler: opLDA, cycles: 5, mode: indirectY, mnemonic: "LDA") # 5+
  opcodeTable[0xB4] = OpcodeInfo(fixedCycles: true, handler: opLDY, cycles: 4, mode: zeroPageX, mnemonic: "LDY")
  opcodeTable[0xB5] = OpcodeInfo(fixedCycles: true, handler: opLDA, cycles: 4, mode: zeroPageX, mnemonic: "LDA")
  opcodeTable[0xB6] = OpcodeInfo(fixedCycles: true, handler: opLDX, cycles: 4, mode: zeroPageY, mnemonic: "LDX")
  opcodeTable[0xB8] = OpcodeInfo(fixedCycles: true, handler: opCLV, cycles: 2, mode: implied, mnemonic: "CLV")
  opcodeTable[0xB9] = OpcodeInfo(fixedCycles: false, handler: opLDA, cycles: 4, mode: absoluteY, mnemonic: "LDA") # 4+
  opcodeTable[0xBA] = OpcodeInfo(fixedCycles: true, handler: opTSX, cycles: 2, mode: implied, mnemonic: "TSX")
  opcodeTable[0xBC] = OpcodeInfo(fixedCycles: false, handler: opLDY, cycles: 4, mode: absoluteX, mnemonic: "LDY") # 4+
  opcodeTable[0xBD] = OpcodeInfo(fixedCycles: false, handler: opLDA, cycles: 4, mode: absoluteX, mnemonic: "LDA") # 4+
  opcodeTable[0xBE] = OpcodeInfo(fixedCycles: false, handler: opLDX, cycles: 4, mode: absoluteY, mnemonic: "LDX") # 4+
  opcodeTable[0xC0] = OpcodeInfo(fixedCycles: true, handler: opCPY, cycles: 2, mode: immediate, mnemonic: "CPY")
  opcodeTable[0xC1] = OpcodeInfo(fixedCycles: true, handler: opCMP, cycles: 6, mode: indirectX, mnemonic: "CMP")
  opcodeTable[0xC4] = OpcodeInfo(fixedCycles: true, handler: opCPY, cycles: 3, mode: zeroPage, mnemonic: "CPY")
  opcodeTable[0xC5] = OpcodeInfo(fixedCycles: true, handler: opCMP, cycles: 3, mode: zeroPage, mnemonic: "CMP")
  opcodeTable[0xC6] = OpcodeInfo(fixedCycles: true, handler: opDEC, cycles: 5, mode: zeroPage, mnemonic: "DEC")
  opcodeTable[0xC8] = OpcodeInfo(fixedCycles: true, handler: opINY, cycles: 2, mode: implied, mnemonic: "INY")
  opcodeTable[0xC9] = OpcodeInfo(fixedCycles: true, handler: opCMP, cycles: 2, mode: immediate, mnemonic: "CMP")
  opcodeTable[0xCA] = OpcodeInfo(fixedCycles: true, handler: opDEX, cycles: 2, mode: implied, mnemonic: "DEX")
  opcodeTable[0xCC] = OpcodeInfo(fixedCycles: true, handler: opCPY, cycles: 4, mode: absolute, mnemonic: "CPY")
  opcodeTable[0xCD] = OpcodeInfo(fixedCycles: true, handler: opCMP, cycles: 4, mode: absolute, mnemonic: "CMP")
  opcodeTable[0xCE] = OpcodeInfo(fixedCycles: true, handler: opDEC, cycles: 6, mode: absolute, mnemonic: "DEC")
  opcodeTable[0xD0] = OpcodeInfo(fixedCycles: false, handler: opBNE, cycles: 2, mode: relative, mnemonic: "BNE") # 2++
  opcodeTable[0xD1] = OpcodeInfo(fixedCycles: false, handler: opCMP, cycles: 5, mode: indirectY, mnemonic: "CMP") # 5+
  opcodeTable[0xD5] = OpcodeInfo(fixedCycles: true, handler: opCMP, cycles: 4, mode: zeroPageX, mnemonic: "CMP")
  opcodeTable[0xD6] = OpcodeInfo(fixedCycles: true, handler: opDEC, cycles: 6, mode: zeroPageX, mnemonic: "DEC")
  opcodeTable[0xD8] = OpcodeInfo(fixedCycles: true, handler: opCLD, cycles: 2, mode: implied, mnemonic: "CLD")
  opcodeTable[0xD9] = OpcodeInfo(fixedCycles: false, handler: opCMP, cycles: 4, mode: absoluteY, mnemonic: "CMP") # 4+
  opcodeTable[0xDD] = OpcodeInfo(fixedCycles: false, handler: opCMP, cycles: 4, mode: absoluteX, mnemonic: "CMP") # 4+
  opcodeTable[0xDE] = OpcodeInfo(fixedCycles: true, handler: opDEC, cycles: 7, mode: absoluteX, mnemonic: "DEC")
  opcodeTable[0xE0] = OpcodeInfo(fixedCycles: true, handler: opCPX, cycles: 2, mode: immediate, mnemonic: "CPX")
  opcodeTable[0xE1] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 6, mode: indirectX, mnemonic: "SBC")
  opcodeTable[0xE4] = OpcodeInfo(fixedCycles: true, handler: opCPX, cycles: 3, mode: zeroPage, mnemonic: "CPX")
  opcodeTable[0xE5] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 3, mode: zeroPage, mnemonic: "SBC")
  opcodeTable[0xE6] = OpcodeInfo(fixedCycles: true, handler: opINC, cycles: 5, mode: zeroPage, mnemonic: "INC")
  opcodeTable[0xE8] = OpcodeInfo(fixedCycles: true, handler: opINX, cycles: 2, mode: implied, mnemonic: "INX")
  opcodeTable[0xE9] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 2, mode: immediate, mnemonic: "SBC")
  opcodeTable[0xEA] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0xEC] = OpcodeInfo(fixedCycles: true, handler: opCPX, cycles: 4, mode: absolute, mnemonic: "CPX")
  opcodeTable[0xED] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 4, mode: absolute, mnemonic: "SBC")
  opcodeTable[0xEE] = OpcodeInfo(fixedCycles: true, handler: opINC, cycles: 6, mode: absolute, mnemonic: "INC")
  opcodeTable[0xF0] = OpcodeInfo(fixedCycles: false, handler: opBEQ, cycles: 2, mode: relative, mnemonic: "BEQ") # 2++
  opcodeTable[0xF1] = OpcodeInfo(fixedCycles: false, handler: opSBC, cycles: 5, mode: indirectY, mnemonic: "SBC") # 5+
  opcodeTable[0xF5] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 4, mode: zeroPageX, mnemonic: "SBC")
  opcodeTable[0xF6] = OpcodeInfo(fixedCycles: true, handler: opINC, cycles: 6, mode: zeroPageX, mnemonic: "INC")
  opcodeTable[0xF8] = OpcodeInfo(fixedCycles: true, handler: opSED, cycles: 2, mode: implied, mnemonic: "SED")
  opcodeTable[0xF9] = OpcodeInfo(fixedCycles: false, handler: opSBC, cycles: 4, mode: absoluteY, mnemonic: "SBC") # 4+
  opcodeTable[0xFD] = OpcodeInfo(fixedCycles: false, handler: opSBC, cycles: 4, mode: absoluteX, mnemonic: "SBC") # 4+
  opcodeTable[0xFE] = OpcodeInfo(fixedCycles: true, handler: opINC, cycles: 7, mode: absoluteX, mnemonic: "INC")

  # Unofficial Opcodes (Based on common definitions, e.g., oxyron.de)
  opcodeTable[0x02] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x03] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 8, mode: indirectX, mnemonic: "SLO")
  opcodeTable[0x04] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 3, mode: zeroPage, mnemonic: "NOP") # DOP
  opcodeTable[0x07] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 5, mode: zeroPage, mnemonic: "SLO")
  opcodeTable[0x0B] = OpcodeInfo(fixedCycles: true, handler: opANC, cycles: 2, mode: immediate, mnemonic: "ANC")
  opcodeTable[0x0C] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: absolute, mnemonic: "NOP") # TOP
  opcodeTable[0x0F] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 6, mode: absolute, mnemonic: "SLO")
  opcodeTable[0x12] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x13] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 8, mode: indirectY, mnemonic: "SLO")
  opcodeTable[0x14] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0x17] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 6, mode: zeroPageX, mnemonic: "SLO")
  opcodeTable[0x1A] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0x1B] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 7, mode: absoluteY, mnemonic: "SLO")
  opcodeTable[0x1C] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0x1F] = OpcodeInfo(fixedCycles: true, handler: opSLO, cycles: 7, mode: absoluteX, mnemonic: "SLO")
  opcodeTable[0x22] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x23] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 8, mode: indirectX, mnemonic: "RLA")
  opcodeTable[0x27] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 5, mode: zeroPage, mnemonic: "RLA")
  opcodeTable[0x2B] = OpcodeInfo(fixedCycles: true, handler: opANC, cycles: 2, mode: immediate, mnemonic: "ANC") # Same as 0B
  opcodeTable[0x2F] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 6, mode: absolute, mnemonic: "RLA")
  opcodeTable[0x32] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x33] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 8, mode: indirectY, mnemonic: "RLA")
  opcodeTable[0x34] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0x37] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 6, mode: zeroPageX, mnemonic: "RLA")
  opcodeTable[0x3A] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0x3B] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 7, mode: absoluteY, mnemonic: "RLA")
  opcodeTable[0x3C] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0x3F] = OpcodeInfo(fixedCycles: true, handler: opRLA, cycles: 7, mode: absoluteX, mnemonic: "RLA")
  opcodeTable[0x42] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x43] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 8, mode: indirectX, mnemonic: "SRE")
  opcodeTable[0x44] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 3, mode: zeroPage, mnemonic: "NOP") # DOP
  opcodeTable[0x47] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 5, mode: zeroPage, mnemonic: "SRE")
  opcodeTable[0x4B] = OpcodeInfo(fixedCycles: true, handler: opALR, cycles: 2, mode: immediate, mnemonic: "ALR")
  opcodeTable[0x4F] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 6, mode: absolute, mnemonic: "SRE")
  opcodeTable[0x52] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x53] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 8, mode: indirectY, mnemonic: "SRE")
  opcodeTable[0x54] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0x57] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 6, mode: zeroPageX, mnemonic: "SRE")
  opcodeTable[0x5A] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0x5B] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 7, mode: absoluteY, mnemonic: "SRE")
  opcodeTable[0x5C] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0x5F] = OpcodeInfo(fixedCycles: true, handler: opSRE, cycles: 7, mode: absoluteX, mnemonic: "SRE")
  opcodeTable[0x62] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x63] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 8, mode: indirectX, mnemonic: "RRA")
  opcodeTable[0x64] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 3, mode: zeroPage, mnemonic: "NOP") # DOP
  opcodeTable[0x67] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 5, mode: zeroPage, mnemonic: "RRA")
  opcodeTable[0x6B] = OpcodeInfo(fixedCycles: true, handler: opARR, cycles: 2, mode: immediate, mnemonic: "ARR")
  opcodeTable[0x6F] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 6, mode: absolute, mnemonic: "RRA")
  opcodeTable[0x72] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x73] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 8, mode: indirectY, mnemonic: "RRA")
  opcodeTable[0x74] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0x77] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 6, mode: zeroPageX, mnemonic: "RRA")
  opcodeTable[0x7A] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0x7B] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 7, mode: absoluteY, mnemonic: "RRA")
  opcodeTable[0x7C] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0x7F] = OpcodeInfo(fixedCycles: true, handler: opRRA, cycles: 7, mode: absoluteX, mnemonic: "RRA")
  opcodeTable[0x80] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: immediate, mnemonic: "NOP") # DOP
  opcodeTable[0x82] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: immediate, mnemonic: "NOP") # DOP
  opcodeTable[0x83] = OpcodeInfo(fixedCycles: true, handler: opAXS, cycles: 6, mode: indirectX, mnemonic: "AXS") # SAX
  opcodeTable[0x87] = OpcodeInfo(fixedCycles: true, handler: opAXS, cycles: 3, mode: zeroPage, mnemonic: "AXS") # SAX
  opcodeTable[0x89] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: immediate, mnemonic: "NOP") # DOP
  opcodeTable[0x8B] = OpcodeInfo(fixedCycles: true, handler: opXAA, cycles: 2, mode: immediate, mnemonic: "XAA") # Highly unstable, often NOP
  opcodeTable[0x8F] = OpcodeInfo(fixedCycles: true, handler: opAXS, cycles: 4, mode: absolute, mnemonic: "AXS") # SAX
  opcodeTable[0x92] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0x93] = OpcodeInfo(fixedCycles: true, handler: opTAS, cycles: 6, mode: indirectY, mnemonic: "TAS") # SHA/AHX
  opcodeTable[0x97] = OpcodeInfo(fixedCycles: true, handler: opAXS, cycles: 4, mode: zeroPageY, mnemonic: "AXS") # SAX
  opcodeTable[0x9B] = OpcodeInfo(fixedCycles: true, handler: opTAS, cycles: 5, mode: absoluteY, mnemonic: "TAS") # SHS/XAS
  opcodeTable[0x9C] = OpcodeInfo(fixedCycles: true, handler: opSHY, cycles: 5, mode: absoluteX, mnemonic: "SHY") # SYA
  opcodeTable[0x9E] = OpcodeInfo(fixedCycles: true, handler: opSHX, cycles: 5, mode: absoluteY, mnemonic: "SHX") # SXA
  opcodeTable[0x9F] = OpcodeInfo(fixedCycles: true, handler: opTAS, cycles: 5, mode: absoluteY, mnemonic: "TAS") # SHA/AHX
  opcodeTable[0xA3] = OpcodeInfo(fixedCycles: true, handler: opLAX, cycles: 6, mode: indirectX, mnemonic: "LAX")
  opcodeTable[0xA7] = OpcodeInfo(fixedCycles: true, handler: opLAX, cycles: 3, mode: zeroPage, mnemonic: "LAX")
  opcodeTable[0xAB] = OpcodeInfo(fixedCycles: true, handler: opLAX, cycles: 2, mode: immediate, mnemonic: "LAX") # OAL/LXA
  opcodeTable[0xAF] = OpcodeInfo(fixedCycles: true, handler: opLAX, cycles: 4, mode: absolute, mnemonic: "LAX")
  opcodeTable[0xB2] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0xB3] = OpcodeInfo(fixedCycles: false, handler: opLAX, cycles: 5, mode: indirectY, mnemonic: "LAX") # 5+
  opcodeTable[0xB7] = OpcodeInfo(fixedCycles: true, handler: opLAX, cycles: 4, mode: zeroPageY, mnemonic: "LAX")
  opcodeTable[0xBB] = OpcodeInfo(fixedCycles: false, handler: opLAS, cycles: 4, mode: absoluteY, mnemonic: "LAS") # 4+
  opcodeTable[0xBF] = OpcodeInfo(fixedCycles: false, handler: opLAX, cycles: 4, mode: absoluteY, mnemonic: "LAX") # 4+
  opcodeTable[0xC2] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: immediate, mnemonic: "NOP") # DOP
  opcodeTable[0xC3] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 8, mode: indirectX, mnemonic: "DCP")
  opcodeTable[0xC7] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 5, mode: zeroPage, mnemonic: "DCP")
  opcodeTable[0xCB] = OpcodeInfo(fixedCycles: true, handler: opAXS, cycles: 2, mode: immediate, mnemonic: "AXS") # SBX/ATX
  opcodeTable[0xCF] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 6, mode: absolute, mnemonic: "DCP")
  opcodeTable[0xD2] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0xD3] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 8, mode: indirectY, mnemonic: "DCP")
  opcodeTable[0xD4] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0xD7] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 6, mode: zeroPageX, mnemonic: "DCP")
  opcodeTable[0xDA] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0xDB] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 7, mode: absoluteY, mnemonic: "DCP")
  opcodeTable[0xDC] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0xDF] = OpcodeInfo(fixedCycles: true, handler: opDCP, cycles: 7, mode: absoluteX, mnemonic: "DCP")
  opcodeTable[0xE2] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: immediate, mnemonic: "NOP") # DOP
  opcodeTable[0xE3] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 8, mode: indirectX, mnemonic: "ISC")
  opcodeTable[0xE7] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 5, mode: zeroPage, mnemonic: "ISC")
  opcodeTable[0xEB] = OpcodeInfo(fixedCycles: true, handler: opSBC, cycles: 2, mode: immediate, mnemonic: "SBC") # Unofficial SBC
  opcodeTable[0xEF] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 6, mode: absolute, mnemonic: "ISC")
  opcodeTable[0xF2] = OpcodeInfo(fixedCycles: true, handler: opKIL, cycles: 2, mode: implied, mnemonic: "KIL")
  opcodeTable[0xF3] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 8, mode: indirectY, mnemonic: "ISC")
  opcodeTable[0xF4] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 4, mode: zeroPageX, mnemonic: "NOP") # DOP
  opcodeTable[0xF7] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 6, mode: zeroPageX, mnemonic: "ISC")
  opcodeTable[0xFA] = OpcodeInfo(fixedCycles: true, handler: opNOP, cycles: 2, mode: implied, mnemonic: "NOP")
  opcodeTable[0xFB] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 7, mode: absoluteY, mnemonic: "ISC")
  opcodeTable[0xFC] = OpcodeInfo(fixedCycles: false, handler: opNOP, cycles: 4, mode: absoluteX, mnemonic: "NOP") # TOP (4+)
  opcodeTable[0xFF] = OpcodeInfo(fixedCycles: true, handler: opISC, cycles: 7, mode: absoluteX, mnemonic: "ISC")

# Note: The specific handler implementations (like opORA_imm, opLDA_zp, etc.)
# should be removed in a later step (Step 5) after testing confirms the
# generic handlers and this table work correctly.
