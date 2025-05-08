import ../types
import ../addressing
import ../flags
import ../opcode_utils
import ../memory
import ../stack
import ../utils
import strformat
import strutils
proc opBEQ*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic BEQ handler (Branch if Equal - Z flag set).
  let instruction = cpu.memory[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Assumes mode is Relative
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  opcode_utils.handleBranch(cpu, info, result, cpu.Z)


proc opBMI*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for BMI (Branch if Minus) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BMI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBNE*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic BNE handler (Branch if Not Equal - Z flag clear).
  let instruction = cpu.memory[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Assumes mode is Relative
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  opcode_utils.handleBranch(cpu, info, result, not cpu.Z)




proc opBVC*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for BVC (Branch if Overflow Clear) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BVC ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBVS*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for BVS (Branch if Overflow Set) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BVS ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err

proc opBPL*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for BPL (Branch if Plus - N flag clear) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode BPL ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err



proc opBRK*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic BRK handler (Force Break).
  ## Action: Push PC+2, Push P (with B=1), Set I=1, PC = ($FFFE)
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (though it's always 0x00 for BRK)
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Get info (mainly for cycles)

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
  echo "BRK Debug - IRQ Vector: $", toHex(0xFFFE'u16,4), "=", toHex(lowByte,2), " $", toHex(0xFFFF'u16,4), "=", toHex(highByte,2)
  let newPC = (uint16(highByte) shl 8) or uint16(lowByte)
  echo "BRK Debug - New PC: $", toHex(newPC,4)
  cpu.PC = newPC

  # 6. Update CPU cycles (BRK always takes 7 cycles)
  cpu.cycles += uint16(info.cycles) # Use cycles from table (should be 7)


proc opJMP*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for JMP (Jump) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode JMP ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opJSR*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic JSR handler (Jump to Subroutine).
  ## Action: Push PC+2, PC = Address
  let instruction = cpu.memory[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Get info (mode=Absolute, cycles=6)
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




proc opRTS*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic RTS handler (Return from Subroutine).
  ## Action: Pull PC from stack, PC = Pulled PC + 1
  let instruction = cpu.memory[cpu.PC] # Fetch instruction (always 0x60 for RTS)
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Get info (mode=Implied, cycles=6)

  # Pull the return address (minus 1) from the stack
  # Stack order: high byte pushed first by JSR, so pull low byte first
  let lowByte = pull(cpu)
  let highByte = pull(cpu)
  let returnAddress = (uint16(highByte) shl 8) or uint16(lowByte)

  # Set the Program Counter to the address *after* the JSR instruction
  cpu.PC = returnAddress + 1

  # Update CPU cycles (RTS always takes 6 cycles)
  cpu.cycles += uint16(info.cycles) # Use cycles from table (should be 6)


proc opRTI*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for RTI (Return from Interrupt) - Not Implemented
  let instruction = cpu.memory[cpu.PC]
  let err = UnimplementedOpcodeError(
    msg: "Opcode RTI ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opBCC*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic BCC handler (Branch if Carry Clear).
  ## Action: Branch if C flag is clear.
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  opcode_utils.handleBranch(cpu, info, result, not cpu.C)


proc opBCS*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic BCS handler (Branch if Carry Set).
  ## Action: Branch if C flag is set.
  let result = resolveAddressingMode(cpu, info.mode) # Gets relative offset
  opcode_utils.handleBranch(cpu, info, result, cpu.C)