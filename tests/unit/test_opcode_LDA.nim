import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "LDA Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "LDA immediate mode sets accumulator and flags - positive value":
    # Setup LDA #$42 (A9 42)
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x42  # Value to load
    cpu.cycles = 0  # Reset cycles
    
    # Execute just one instruction
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)
    
    check:
      cpu.A == 0x42        # Accumulator has value
      not cpu.Z            # Zero flag clear (0x42 != 0)
      not cpu.N            # Negative flag clear (bit 7 = 0)
      cpu.PC == 0x302     # PC advanced past instruction + operand
      cpu.cycles == 2      # LDA immediate takes 2 cycles

  test "LDA immediate mode sets zero flag":
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x00  # Load zero
    
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)
    
    check cpu.Z == true    # Zero flag should be set

  test "LDA immediate mode sets negative flag":
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x80  # Load negative value (bit 7 set)
    
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)
    
    check:
      cpu.N == true        # Negative flag should be set
      not cpu.Z            # Zero flag should be clear

  test "LDA (Indirect,X) - Basic Operation":
    # Setup: LDA ($40,X) where X=0x04
    # Zero page address = $40 + $04 = $44
    # Effective address stored at $0044/$0045 is $1234
    # Value at $1234 is $AA
    cpu.PC = 0x0200
    cpu.X = 0x04
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0200] = 0xA1  # LDA (Indirect,X)
    mem.mem[0x0201] = 0x40  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x0044] = 0x34  # Low byte of effective address ($1234)
    mem.mem[0x0045] = 0x12  # High byte of effective address ($1234)

    # Setup the value at the effective address
    mem.mem[0x1234] = 0xAA

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xAA        # Accumulator updated
      not cpu.Z            # Zero flag clear (AA != 0)
      cpu.N == true        # Negative flag set (bit 7 of AA is 1)
      cpu.PC == 0x0202     # PC advanced by 2
      cpu.cycles == 6      # LDA (Indirect,X) takes 6 cycles

  test "LDA ZeroPage - Basic Operation":
    # Setup: LDA $30
    # Value at $0030 is $BB
    cpu.PC = 0x0900
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear N, Z initially
    cpu.cycles = 0

    mem.mem[0x0900] = 0xA5  # LDA ZeroPage
    mem.mem[0x0901] = 0x30  # Zero page address operand

    # Setup the value in zero page
    mem.mem[0x0030] = 0xBB

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xBB        # Accumulator updated
      not cpu.Z            # Zero flag clear (BB != 0)
      cpu.N == true        # Negative flag set (bit 7 of BB is 1)
      cpu.PC == 0x0902     # PC advanced by 2
      cpu.cycles == 3      # LDA ZeroPage takes 3 cycles

  test "LDA (Indirect),Y - Basic Operation, No Page Cross":
    # Setup: LDA ($80),Y where Y=0x10
    # Base address pointer at $0080/$0081 is $1000
    # Effective address = $1000 + Y = $1010
    # Value at $1010 is $CC
    cpu.PC = 0x0A00
    cpu.Y = 0x10
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0A00] = 0xB1  # LDA (Indirect),Y
    mem.mem[0x0A01] = 0x80  # Zero page address containing base pointer

    # Setup the base address pointer in zero page
    mem.mem[0x0080] = 0x00  # Low byte of base address ($1000)
    mem.mem[0x0081] = 0x10  # High byte of base address ($1000)

    # Setup the value at the effective address
    mem.mem[0x1010] = 0xCC

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xCC        # Accumulator updated
      not cpu.Z            # Zero flag clear (CC != 0)
      cpu.N == true        # Negative flag set (bit 7 of CC is 1)
      cpu.PC == 0x0A02     # PC advanced by 2
      cpu.cycles == 5      # LDA (Indirect),Y takes 5 cycles (no page cross)

  test "LDA (Indirect),Y - Page Cross":
    # Setup: LDA ($80),Y where Y=0xFF
    # Base address pointer at $0080/$0081 is $1001
    # Effective address = $1001 + Y = $1001 + $FF = $1100 (page crossed)
    # Value at $1100 is $DD
    cpu.PC = 0x0B00
    cpu.Y = 0xFF
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0B00] = 0xB1  # LDA (Indirect),Y
    mem.mem[0x0B01] = 0x80  # Zero page address containing base pointer

    # Setup the base address pointer in zero page
    mem.mem[0x0080] = 0x01  # Low byte of base address ($1001)
    mem.mem[0x0081] = 0x10  # High byte of base address ($1001)

    # Setup the value at the effective address
    mem.mem[0x1100] = 0xDD

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xDD        # Accumulator updated
      not cpu.Z            # Zero flag clear (DD != 0)
      cpu.N == true        # Negative flag set (bit 7 of DD is 1)
      cpu.PC == 0x0B02     # PC advanced by 2
      cpu.cycles == 6      # LDA (Indirect),Y takes 6 cycles (5 + 1 for page cross)

  test "LDA Absolute,X - Basic Operation, No Page Cross":
    # Setup: LDA $1000,X where X=0x10
    # Effective address = $1000 + X = $1010
    # Value at $1010 is $EE
    cpu.PC = 0x0C00
    cpu.X = 0x10
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0C00] = 0xBD  # LDA Absolute,X
    mem.mem[0x0C01] = 0x00  # Low byte of base address
    mem.mem[0x0C02] = 0x10  # High byte of base address

    # Setup the value at the effective address
    mem.mem[0x1010] = 0xEE

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xEE        # Accumulator updated
      not cpu.Z            # Zero flag clear (EE != 0)
      cpu.N == true        # Negative flag set (bit 7 of EE is 1)
      cpu.PC == 0x0C03     # PC advanced by 3
      cpu.cycles == 4      # LDA Absolute,X takes 4 cycles (no page cross)

  test "LDA Absolute,X - Page Cross":
    # Setup: LDA $10FF,X where X=0x01
    # Effective address = $10FF + X = $1100 (page crossed)
    # Value at $1100 is $FF
    cpu.PC = 0x0D00
    cpu.X = 0x01
    cpu.A = 0x00 # Clear A initially
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0D00] = 0xBD  # LDA Absolute,X
    mem.mem[0x0D01] = 0xFF  # Low byte of base address
    mem.mem[0x0D02] = 0x10  # High byte of base address

    # Setup the value at the effective address
    mem.mem[0x1100] = 0xFF

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu, info)

    check:
      cpu.A == 0xFF        # Accumulator updated
      not cpu.Z            # Zero flag clear (FF != 0)
      cpu.N == true        # Negative flag set (bit 7 of FF is 1)
      cpu.PC == 0x0D03     # PC advanced by 3
      cpu.cycles == 5      # LDA Absolute,X takes 5 cycles (4 + 1 for page cross)