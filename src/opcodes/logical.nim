import ../types
import ../addressing
import ../opcode_utils
import strutils

proc opORA*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic ORA handler.
  # let instruction = cpu.memory.mem[cpu.PC] # instruction is not used
  # let info = opcodeTable[instruction] # info is now passed as a parameter
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to operate on
  let value = opcode_utils.fetchOperandValue(cpu, info, result)

  # Perform ORA logic
  cpu.A = cpu.A or value

  # Update flags
  opcode_utils.updateZNFlags(cpu, cpu.A)

  # Update Program Counter and CPU cycles
  opcode_utils.updatePCAndCycles(cpu, info, result)


proc opAND*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic AND handler.
  # let instruction = cpu.memory.mem[cpu.PC] # instruction is not used
  # let info = opcodeTable[instruction] # info is now passed as a parameter
  let result = resolveAddressingMode(cpu, info.mode)

  # Fetch the value to operate on
  let value = opcode_utils.fetchOperandValue(cpu, info, result)

  # Perform AND logic
  cpu.A = cpu.A and value

  # Update flags
  opcode_utils.updateZNFlags(cpu, cpu.A)

  # Update Program Counter and CPU cycles
  opcode_utils.updatePCAndCycles(cpu, info, result)


proc opEOR*(cpu: var CPU, info: OpcodeInfo) =
  ## Stub for EOR (Exclusive OR) - Not Implemented
  let instruction = cpu.memory.mem[cpu.PC]
  let err = UnimplementedOpcodeError( # opcodeTable parameter not used in stub
    msg: "Opcode EOR ($" & toHex(instruction, 2) & ") not implemented",
    opcode: instruction,
    pc: cpu.PC
  )
  raise err


proc opANC*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic ANC handler (unofficial).
  ## Action: A = A & Immediate; C = N
  # let instruction = cpu.memory.mem[cpu.PC] # instruction is not used
  # let info = opcodeTable[instruction] # info is now passed as a parameter # Assumes mode is Immediate

  # ANC only uses Immediate mode
  # We could add an assertion here: assert info.mode == immediate
  let result = resolveAddressingMode(cpu, immediate) # Force immediate mode resolution
  let value = result.value

  # Perform AND logic
  cpu.A = cpu.A and value

  # Update Z and N flags based on the result in A
  opcode_utils.updateZNFlags(cpu, cpu.A)

  # Set Carry flag based on the N flag (bit 7 of the result)
  cpu.C = cpu.N

  # Update Program Counter and CPU cycles
  opcode_utils.updatePCAndCycles(cpu, info, result)