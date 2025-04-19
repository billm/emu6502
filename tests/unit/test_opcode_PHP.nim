import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "PHP Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "PHP Implied (0x08) pushes status with B and U set":
    # Setup initial state
    cpu.PC = 0x0900
    cpu.SP = 0xFD  # Start SP somewhere in the middle
    cpu.setFlags(0b10100001) # N=1, V=0, U=1(ignored), B=0(ignored), D=0, I=0, Z=0, C=1 => $A1
    cpu.cycles = 10 # Initial cycles
    let initialSP = cpu.SP
    let initialStatus = cpu.flags() # Should be $A1
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Place PHP opcode
    mem.mem[cpu.PC] = 0x08

    # Expected pushed value: initialStatus | 0x30 = $A1 | $30 = $B1
    let expectedPushedValue = initialStatus or 0x30'u8

    # Execute (will fail until implemented)
    try:
      let info = opcodeTable[mem.mem[cpu.PC]]
      if info.handler != nil:
        info.handler(cpu, info)
      else:
        # If handler is nil, the test might expect UnimplementedOpcodeError
        # or simply proceed if the goal is just to add the test structure.
        # The checks below will fail if the handler *is* implemented incorrectly.
        discard
    except UnimplementedOpcodeError:
      # This is the expected path if the handler is truly nil
      discard # Allow test to proceed to checks for when it *is* implemented

    # Checks
    check:
      cpu.SP == initialSP - 1             # SP decremented
      mem.mem[0x0100 + initialSP.uint16] == expectedPushedValue # Correct value pushed ($B1)
      cpu.flags() == initialStatus     # CPU status register unchanged ($A1)
      cpu.PC == initialPC + 1             # PC incremented by 1
      cpu.cycles == initialCycles + 3     # Cycles incremented by 3