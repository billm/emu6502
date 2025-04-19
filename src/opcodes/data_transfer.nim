import ../memory, ../types, ../addressing, ../flags, ../opcode_utils
proc opLDA*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic LDA handler (Load Accumulator).
  ## Action: A = M
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.A = value
  opcode_utils.updateZNFlags(cpu, cpu.A)

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opLDX*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic LDX handler (Load Index Register X).
  ## Action: X = M
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.X = value
  opcode_utils.updateZNFlags(cpu, cpu.X)

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opLDY*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic LDY handler (Load Index Register Y).
  ## Action: Y = M
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.Y = value
  opcode_utils.updateZNFlags(cpu, cpu.Y)

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opSTA*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic STA handler (Store Accumulator).
  ## Action: M = A
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  # STA does not fetch the operand value, it writes to the address
  # let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.memory[result.address] = cpu.A

  # STA does not affect flags

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opSTX*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic STX handler (Store Index Register X).
  ## Action: M = X
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  # STX does not fetch the operand value, it writes to the address
  # let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.memory[result.address] = cpu.X

  # STX does not affect flags

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opSTY*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic STY handler (Store Index Register Y).
  ## Action: M = Y
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  let result = addressing.resolveAddressingMode(cpu, info.mode)
  # STY does not fetch the operand value, it writes to the address
  # let value = opcode_utils.fetchOperandValue(cpu, info, result)

  cpu.memory[result.address] = cpu.Y

  # STY does not affect flags

  opcode_utils.updatePCAndCycles(cpu, info, result)
proc opTAX*(cpu: var CPU, info: OpcodeInfo) =
  ## Generic TAX handler (Transfer Accumulator to X).
  ## Action: X = A
  let instruction = cpu.memory.mem[cpu.PC]
  # let info = opcodeTable[instruction] # info is now passed as a parameter

  cpu.X = cpu.A
  opcode_utils.updateZNFlags(cpu, cpu.X)

  # TAX is an implied mode instruction, 1 byte, 2 cycles
  opcode_utils.updatePCAndCycles(cpu, info, AddressingResult(operandBytes: 0, extraCycles: 0))