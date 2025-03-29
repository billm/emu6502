import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "SLO Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  # --- Tests for Opcode 0x03: SLO (Indirect,X) ---

  test "SLO (Indirect,X) - Basic Operation, No Carry":
    # Setup: SLO ($30,X) where X=0x05
    # Zero page address = $30 + $05 = $35
    # Effective address stored at $0035/$0036 is $C123
    # Initial value M at $C123 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0 (original bit 7 was 0).
    # 2. Write $82 back to $C123.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$C123] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0400
    cpu.X = 0x05
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0400] = 0x03  # SLO (Indirect,X)
    mem.mem[0x0401] = 0x30  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x0035] = 0x23  # Low byte of effective address ($C123)
    mem.mem[0x0036] = 0xC1  # High byte of effective address ($C123)

    # Setup the initial value at the effective address
    mem.mem[0xC123] = 0x41

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[0xC123] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0402          # PC advanced by 2
      cpu.cycles == 8           # SLO (Indirect,X) takes 8 cycles

  test "SLO (Indirect,X) - Sets Carry Flag":
    # Setup: SLO ($A0,X) where X=0x02
    # Zero page address = $A0 + $02 = $A2
    # Effective address stored at $00A2/$00A3 is $D456
    # Initial value M at $D456 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1 (original bit 7 was 1).
    # 2. Write $02 back to $D456.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$D456] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0500
    cpu.X = 0x02
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0

    mem.mem[0x0500] = 0x03  # SLO (Indirect,X)
    mem.mem[0x0501] = 0xA0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00A2] = 0x56  # Low byte of effective address ($D456)
    mem.mem[0x00A3] = 0xD4  # High byte of effective address ($D456)

    # Setup the initial value at the effective address
    mem.mem[0xD456] = 0x81

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[0xD456] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x0502          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  test "SLO (Indirect,X) - Sets Zero Flag":
    # Setup: SLO ($B0,X) where X=0x01
    # Zero page address = $B0 + $01 = $B1
    # Effective address stored at $00B1/$00B2 is $E789
    # Initial value M at $E789 is $00 (00000000)
    # Initial A = $00 (00000000)
    #
    # Action:
    # 1. ASL on M: $00 << 1 = $00. Carry = 0.
    # 2. Write $00 back to $E789.
    # 3. ORA: A = A | shifted M = $00 | $00 = $00
    #
    # Expected State:
    # A = $00
    # Memory[$E789] = $00
    # Flags: N=0, Z=1, C=0
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0600
    cpu.X = 0x01
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x01'u8) # Set N, C initially
    cpu.cycles = 0

    mem.mem[0x0600] = 0x03  # SLO (Indirect,X)
    mem.mem[0x0601] = 0xB0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00B1] = 0x89  # Low byte of effective address ($E789)
    mem.mem[0x00B2] = 0xE7  # High byte of effective address ($E789)

    # Setup the initial value at the effective address
    mem.mem[0xE789] = 0x00

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[0xE789] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x0602          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  test "SLO (Indirect,X) - Zero Page Wrap-around":
    # Setup: SLO ($FE,X) where X=0x03
    # Zero page address = $FE + $03 = $101 -> wraps to $01
    # Effective address stored at $0001/$0002 is $BEEF
    # Initial value M at $BEEF is $C0 (11000000)
    # Initial A = $03 (00000011)
    #
    # Action:
    # 1. ASL on M: $C0 << 1 = $80 (10000000). Carry = 1.
    # 2. Write $80 back to $BEEF.
    # 3. ORA: A = A | shifted M = $03 | $80 = $83 (10000011)
    #
    # Expected State:
    # A = $83
    # Memory[$BEEF] = $80
    # Flags: N=1, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0700
    cpu.X = 0x03
    cpu.A = 0x03
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0700] = 0x03  # SLO (Indirect,X)
    mem.mem[0x0701] = 0xFE  # Zero page base address

    # Setup the indirect address lookup in zero page (with wrap)
    mem.mem[0x0001] = 0xEF  # Low byte of effective address ($BEEF)
    mem.mem[0x0002] = 0xBE  # High byte of effective address ($BEEF)

    # Setup the initial value at the effective address
    mem.mem[0xBEEF] = 0xC0

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x83             # Accumulator updated (03 | 80 = 83)
      mem.mem[0xBEEF] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      cpu.C == true             # Carry flag set
      cpu.PC == 0x0702          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  # --- Tests for Opcode 0x07: SLO ZeroPage (Unofficial) ---

  test "SLO ZeroPage - Basic Operation, No Carry":
    # Setup: SLO $42 (07 42)
    # Value M at $0042 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0 (original bit 7 was 0).
    # 2. Write $82 back to $0042.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$0042] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 5
    cpu.PC = 0x0D00
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let zpAddr = 0x42'u8

    mem.mem[cpu.PC] = 0x07     # SLO ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x41 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[zpAddr.uint16] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0D02          # PC advanced by 2
      cpu.cycles == 5           # SLO ZeroPage takes 5 cycles

  test "SLO ZeroPage - Sets Carry Flag":
    # Setup: SLO $55 (07 55)
    # Value M at $0055 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1 (original bit 7 was 1).
    # 2. Write $02 back to $0055.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$0055] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 5
    cpu.PC = 0x0E00
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpAddr = 0x55'u8

    mem.mem[cpu.PC] = 0x07     # SLO ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x81 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[zpAddr.uint16] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x0E02          # PC advanced by 2
      cpu.cycles == 5           # Cycles correct

  test "SLO ZeroPage - Sets Zero Flag":
    # Setup: SLO $68 (07 68)
    # Value M at $0068 is $80 (10000000) -> ASL -> $00, C=1
    # Initial A = $00
    #
    # Action:
    # 1. ASL on M: $80 << 1 = $00. Carry = 1.
    # 2. Write $00 back to $0068.
    # 3. ORA: A = A | shifted M = $00 | $00 = $00
    #
    # Expected State:
    # A = $00
    # Memory[$0068] = $00
    # Flags: N=0, Z=1, C=1
    # PC = PC + 2
    # Cycles = Cycles + 5
    cpu.PC = 0x0F00
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially, clear C
    cpu.cycles = 0
    let zpAddr = 0x68'u8

    mem.mem[cpu.PC] = 0x07     # SLO ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x80 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[zpAddr.uint16] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      cpu.C == true             # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x0F02          # PC advanced by 2
      cpu.cycles == 5           # Cycles correct

  test "SLO ZeroPage - Sets Negative Flag":
    # Setup: SLO $7A (07 7A)
    # Value M at $007A is $40 (01000000) -> ASL -> $80, C=0
    # Initial A = $01
    #
    # Action:
    # 1. ASL on M: $40 << 1 = $80. Carry = 0.
    # 2. Write $80 back to $007A.
    # 3. ORA: A = A | shifted M = $01 | $80 = $81
    #
    # Expected State:
    # A = $81
    # Memory[$007A] = $80
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 5
    cpu.PC = 0x1000
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0
    let zpAddr = 0x7A'u8

    mem.mem[cpu.PC] = 0x07     # SLO ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x40 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81             # Accumulator updated (01 | 80 = 81)
      mem.mem[zpAddr.uint16] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1002          # PC advanced by 2
      cpu.cycles == 5           # Cycles correct

  # --- Tests for Opcode 0x0F: SLO Absolute ---

  test "SLO Absolute - Basic Operation, No Carry, Positive Result":
    # Setup: SLO $C050 (0F 50 C0)
    # Initial value M at $C050 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0 (original bit 7 was 0).
    # 2. Write $82 back to $C050.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$C050] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 6
    cpu.PC = 0x0600
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0600] = 0x0F  # SLO Absolute
    mem.mem[0x0601] = 0x50  # Low byte of address
    mem.mem[0x0602] = 0xC0  # High byte of address

    # Setup the initial value at the effective address
    mem.mem[0xC050] = 0x41

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[0xC050] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0603          # PC advanced by 3
      cpu.cycles == 6           # SLO Absolute takes 6 cycles
