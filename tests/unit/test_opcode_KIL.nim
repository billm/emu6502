import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "KIL Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "KIL Implied (0x02) halts the CPU":
    # Setup KIL (02)
    cpu.PC = 0x400
    mem.mem[0x400] = 0x02  # KIL opcode
    cpu.cycles = 10  # Initial cycle count
    cpu.halted = false # Ensure not halted initially
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Execute - This should fail until implemented
    # It might raise UnimplementedOpcodeError if handler is nil
    try:
      let info = opcodeTable[mem.mem[cpu.PC]]
      if info.handler != nil:
        info.handler(cpu)
      else:
        # Simulate failure if not implemented - real run would raise
        fail()
    except UnimplementedOpcodeError:
      # This is the expected path if the handler is truly nil
      # We still need checks below to ensure *correct* implementation later
      discard # Allow test to proceed to checks for when it *is* implemented
    
    # Checks - These will fail until the opcode is correctly implemented
    check:
      cpu.halted == true                   # CPU should be halted
      cpu.PC == initialPC + 1            # PC should advance by 1
      cpu.cycles == initialCycles + 2    # Cycles should increment by 2

  test "KIL Implied (0x12) halts the CPU":
    # Setup KIL (12)
    cpu.PC = 0x500
    mem.mem[0x500] = 0x12  # KIL opcode
    cpu.cycles = 20  # Initial cycle count
    cpu.halted = false # Ensure not halted initially
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Execute - This should fail until implemented
    # It might raise UnimplementedOpcodeError if handler is nil
    try:
      let info = opcodeTable[mem.mem[cpu.PC]]
      if info.handler != nil:
        info.handler(cpu)
      else:
        # Simulate failure if not implemented - real run would raise
        fail()
    except UnimplementedOpcodeError:
      # This is the expected path if the handler is truly nil
      discard # Allow test to proceed to checks for when it *is* implemented
    
    # Checks - These will fail until the opcode is correctly implemented
    check:
      cpu.halted == true                   # CPU should be halted
      cpu.PC == initialPC + 1            # PC should advance by 1
      cpu.cycles == initialCycles + 2    # Cycles should increment by 2