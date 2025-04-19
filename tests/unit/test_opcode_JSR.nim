import unittest
import ../fixtures/emulator_setup
import ../../src/cpu
import ../../src/opcodes
import ../../src/memory
import ../../src/types
import ../../src/flags

suite "Test Opcode 0x20 - JSR Absolute":
  var cpu: CPU
  var mem: Memory

  setup:
    (cpu, mem) = createEmulator()
    # Ensure opcode table is set up
    setupOpcodeTable()

  test "JSR Absolute - Jumps to address, pushes PC+2, decrements SP, updates cycles":
    # Setup initial state
    cpu.PC = 0x0200
    cpu.SP = 0xFF # Initial Stack Pointer

    # JSR $ABCD
    mem[0x0200] = 0x20 # JSR opcode
    mem[0x0201] = 0xCD # Low byte of target address
    mem[0x0202] = 0xAB # High byte of target address

    # Expected state after JSR
    let expectedPC: uint16 = 0xABCD
    let expectedReturnAddr = cpu.PC + 2 # Address after the JSR instruction (0x0202)
    let expectedReturnAddrHi: uint8 = ((expectedReturnAddr shr 8) and 0xFF).uint8
    let expectedReturnAddrLo: uint8 = (expectedReturnAddr and 0xFF).uint8
    let expectedSP: uint8 = (cpu.SP - 2).uint8
    let initialCycles = cpu.cycles
    let expectedCycles: uint64 = initialCycles + 6

    # Execute the instruction
    let opcode = mem.mem[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    # Assertions
    check cpu.PC == expectedPC
    check cpu.SP == expectedSP

    # Check stack contents (Return address pushed high byte first, then low byte)
    check mem[0x0100 or (cpu.SP.uint16 + 1)] == expectedReturnAddrLo # Check stack page 1 # Stack grows down, so SP+1 is the low byte
    check mem[0x0100 or (cpu.SP.uint16 + 2)] == expectedReturnAddrHi # Check stack page 1 # SP+2 is the high byte

    # Check cycles
    check cpu.cycles == expectedCycles

  test "JSR Absolute - Flags should remain unchanged":
    # Setup initial state with specific flags
    cpu.PC = 0x0300
    cpu.SP = 0xFD
    cpu.N = true
    cpu.V = false
    cpu.B = true # B flag state shouldn't matter but set it for completeness
    cpu.D = false
    cpu.I = true
    cpu.Z = false
    cpu.C = true

    # JSR $BEEF
    mem[0x0300] = 0x20 # JSR opcode
    mem[0x0301] = 0xEF # Low byte
    mem[0x0302] = 0xBE # High byte

    # Store initial flag state
    let initialFlags = cpu.flags() # Use the flags() helper proc

    # Execute the instruction
    let opcode = mem.mem[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    # Assertions
    check cpu.PC == 0xBEEF
    check cpu.SP == 0xFB # 0xFD - 2
    check cpu.flags() == initialFlags # Check if the entire status register is unchanged