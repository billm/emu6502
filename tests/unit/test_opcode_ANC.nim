import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "ANC Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  # --- Tests for Opcode 0x0B: ANC Immediate (Unofficial) ---

  test "ANC Immediate - Basic AND, Positive Result, C=N=0":
    # Setup ANC #$0F (0B 0F)
    # Initial A = $5A (01011010)
    # Immediate M = $0F (00001111)
    # Expected A = $5A & $0F = $0A (00001010)
    # Expected Flags: N=0, Z=0, C=0 (since N=0)
    cpu.PC = 0x0D00
    cpu.A = 0x5A
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x02'u8 or 0x01'u8) # Set N, Z, C initially to ensure they are cleared/set correctly
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0B  # ANC immediate
    mem.mem[cpu.PC + 1] = 0x0F  # Value to AND

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x0A        # Accumulator has ANDed value
      not cpu.Z            # Zero flag clear (0x0A != 0)
      not cpu.N            # Negative flag clear (bit 7 = 0)
      not cpu.C            # Carry flag clear (C = N = 0)
      cpu.PC == initialPC + 2     # PC advanced past instruction + operand
      cpu.cycles == initialCycles + 2      # ANC immediate takes 2 cycles

  test "ANC Immediate - Sets Zero Flag, C=N=0":
    # Setup ANC #$F0 (0B F0)
    # Initial A = $0F (00001111)
    # Immediate M = $F0 (11110000)
    # Expected A = $0F & $F0 = $00 (00000000)
    # Expected Flags: N=0, Z=1, C=0 (since N=0)
    cpu.PC = 0x0D00
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x01'u8) # Set N, C initially
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0B  # ANC immediate
    mem.mem[cpu.PC + 1] = 0xF0  # Value to AND

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      not cpu.C            # Carry flag clear (C = N = 0)
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 2

  test "ANC Immediate - Sets Negative Flag, C=N=1":
    # Setup ANC #$FF (0B FF)
    # Initial A = $81 (10000001)
    # Immediate M = $FF (11111111)
    # Expected A = $81 & $FF = $81 (10000001)
    # Expected Flags: N=1, Z=0, C=1 (since N=1)
    cpu.PC = 0x0D00
    cpu.A = 0x81
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially, clear C
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0B  # ANC immediate
    mem.mem[cpu.PC + 1] = 0xFF  # Value to AND

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x81        # Accumulator has ANDed value
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.C == true        # Carry flag set (C = N = 1)
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 2

  test "ANC Immediate - Result Negative, Clears Zero, C=N=1":
    # Setup ANC #$F0 (0B F0)
    # Initial A = $F1 (11110001)
    # Immediate M = $F0 (11110000)
    # Expected A = $F1 & $F0 = $F0 (11110000)
    # Expected Flags: N=1, Z=0, C=1 (since N=1)
    cpu.PC = 0x0D00
    cpu.A = 0xF1
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially, clear C
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0B  # ANC immediate
    mem.mem[cpu.PC + 1] = 0xF0  # Value to AND

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0xF0        # Accumulator has ANDed value
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.C == true        # Carry flag set (C = N = 1)
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 2