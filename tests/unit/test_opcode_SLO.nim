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


  # --- Tests for Opcode 0x13: SLO (Indirect),Y ---

  test "SLO (Indirect),Y - Basic Operation, No Carry, No Page Cross":
    # Setup: SLO ($30),Y where Y=0x05
    # Zero page address = $30
    # Base address stored at $0030/$0031 is $C120
    # Effective address = $C120 + Y = $C120 + $05 = $C125
    # Initial value M at $C125 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0.
    # 2. Write $82 back to $C125.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$C125] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0800
    cpu.Y = 0x05
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0800] = 0x13  # SLO (Indirect),Y
    mem.mem[cpu.PC + 1] = 0x30  # Zero page base address - Corrected typo here

    # Setup the indirect address lookup in zero page
    mem.mem[0x0030] = 0x20  # Low byte of base address ($C120)
    mem.mem[0x0031] = 0xC1  # High byte of base address ($C120)

    # Setup the initial value at the effective address
    mem.mem[0xC125] = 0x41

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[0xC125] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0802          # PC advanced by 2
      cpu.cycles == 8           # SLO (Indirect),Y takes 8 cycles

  test "SLO (Indirect),Y - Sets Carry Flag, No Page Cross":
    # Setup: SLO ($A0),Y where Y=0x02
    # Zero page address = $A0
    # Base address stored at $00A0/$00A1 is $D450
    # Effective address = $D450 + Y = $D450 + $02 = $D452
    # Initial value M at $D452 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1.
    # 2. Write $02 back to $D452.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$D452] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0900
    cpu.Y = 0x02
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0

    mem.mem[0x0900] = 0x13  # SLO (Indirect),Y
    mem.mem[0x0901] = 0xA0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00A0] = 0x50  # Low byte of base address ($D450)
    mem.mem[0x00A1] = 0xD4  # High byte of base address ($D450)

    # Setup the initial value at the effective address
    mem.mem[0xD452] = 0x81

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[0xD452] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x0902          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  test "SLO (Indirect),Y - Sets Zero Flag, No Page Cross":
    # Setup: SLO ($B0),Y where Y=0x01
    # Zero page address = $B0
    # Base address stored at $00B0/$00B1 is $E780
    # Effective address = $E780 + Y = $E780 + $01 = $E781
    # Initial value M at $E781 is $80 (10000000) -> ASL -> $00, C=1
    # Initial A = $00 (00000000)
    #
    # Action:
    # 1. ASL on M: $80 << 1 = $00. Carry = 1.
    # 2. Write $00 back to $E781.
    # 3. ORA: A = A | shifted M = $00 | $00 = $00
    #
    # Expected State:
    # A = $00
    # Memory[$E781] = $00
    # Flags: N=0, Z=1, C=1
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0A00
    cpu.Y = 0x01
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially, clear C
    cpu.cycles = 0

    mem.mem[0x0A00] = 0x13  # SLO (Indirect),Y
    mem.mem[0x0A01] = 0xB0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00B0] = 0x80  # Low byte of base address ($E780)
    mem.mem[0x00B1] = 0xE7  # High byte of base address ($E780)

    # Setup the initial value at the effective address
    mem.mem[0xE781] = 0x80

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[0xE781] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      cpu.C == true             # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x0A02          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  test "SLO (Indirect),Y - Sets Negative Flag, No Page Cross":
    # Setup: SLO ($C0),Y where Y=0x03
    # Zero page address = $C0
    # Base address stored at $00C0/$00C1 is $F010
    # Effective address = $F010 + Y = $F010 + $03 = $F013
    # Initial value M at $F013 is $40 (01000000) -> ASL -> $80, C=0
    # Initial A = $01 (00000001)
    #
    # Action:
    # 1. ASL on M: $40 << 1 = $80. Carry = 0.
    # 2. Write $80 back to $F013.
    # 3. ORA: A = A | shifted M = $01 | $80 = $81
    #
    # Expected State:
    # A = $81
    # Memory[$F013] = $80
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0B00
    cpu.Y = 0x03
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0

    mem.mem[0x0B00] = 0x13  # SLO (Indirect),Y
    mem.mem[0x0B01] = 0xC0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00C0] = 0x10  # Low byte of base address ($F010)
    mem.mem[0x00C1] = 0xF0  # High byte of base address ($F010)

    # Setup the initial value at the effective address
    mem.mem[0xF013] = 0x40

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81             # Accumulator updated (01 | 80 = 81)
      mem.mem[0xF013] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x0B02          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  test "SLO (Indirect),Y - Page Crossing":
    # Setup: SLO ($D0),Y where Y=0x85
    # Zero page address = $D0
    # Base address stored at $00D0/$00D1 is $C180
    # Effective address = $C180 + Y = $C180 + $85 = $C205 (crosses page boundary)
    # Initial value M at $C205 is $01 (00000001)
    # Initial A = $02 (00000010)
    #
    # Action:
    # 1. ASL on M: $01 << 1 = $02. Carry = 0.
    # 2. Write $02 back to $C205.
    # 3. ORA: A = A | shifted M = $02 | $02 = $02
    #
    # Expected State:
    # A = $02
    # Memory[$C205] = $02
    # Flags: N=0, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 8 (Fixed for SLO, even with page cross)
    cpu.PC = 0x0C00
    cpu.Y = 0x85
    cpu.A = 0x02
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x80'u8 or 0x02'u8) # Set N,Z,C initially
    cpu.cycles = 0

    mem.mem[0x0C00] = 0x13  # SLO (Indirect),Y
    mem.mem[0x0C01] = 0xD0  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x00D0] = 0x80  # Low byte of base address ($C180)
    mem.mem[0x00D1] = 0xC1  # High byte of base address ($C180)

    # Setup the initial value at the effective address
    mem.mem[0xC205] = 0x01

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x02             # Accumulator updated (02 | 02 = 02)
      mem.mem[0xC205] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      not cpu.N                 # Negative flag clear
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x0C02          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct (fixed at 8)

  test "SLO (Indirect),Y - Zero Page Pointer Wrap-around":
    # Setup: SLO ($FF),Y where Y=0x04
    # Zero page address = $FF
    # Base address stored at $00FF/$0000 (wraps) is $BEE0
    # Effective address = $BEE0 + Y = $BEE0 + $04 = $BEE4
    # Initial value M at $BEE4 is $C0 (11000000)
    # Initial A = $03 (00000011)
    #
    # Action:
    # 1. ASL on M: $C0 << 1 = $80 (10000000). Carry = 1.
    # 2. Write $80 back to $BEE4.
    # 3. ORA: A = A | shifted M = $03 | $80 = $83 (10000011)
    #
    # Expected State:
    # A = $83
    # Memory[$BEE4] = $80
    # Flags: N=1, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 8
    cpu.PC = 0x0D00
    cpu.Y = 0x04
    cpu.A = 0x03
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0D00] = 0x13  # SLO (Indirect),Y
    mem.mem[0x0D01] = 0xFF  # Zero page base address

    # Setup the indirect address lookup in zero page (with wrap)
    mem.mem[0x00FF] = 0xE0  # Low byte of base address ($BEE0)
    mem.mem[0x0000] = 0xBE  # High byte of base address ($BEE0)

    # Setup the initial value at the effective address
    mem.mem[0xBEE4] = 0xC0

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x83             # Accumulator updated (03 | 80 = 83)
      mem.mem[0xBEE4] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      cpu.C == true             # Carry flag set
      cpu.PC == 0x0D02          # PC advanced by 2
      cpu.cycles == 8           # Cycles correct

  # --- Tests for Opcode 0x17: SLO ZeroPage,X ---

  test "SLO ZeroPage,X - Basic Operation, No Carry":
    # Setup: SLO $40,X where X=0x02 (17 40)
    # Effective address = $40 + $02 = $42
    # Value M at $0042 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0.
    # 2. Write $82 back to $0042.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$0042] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 2
    # Cycles = Cycles + 6
    cpu.PC = 0x0E00
    cpu.X = 0x02
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let zpBaseAddr = 0x40'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF

    mem.mem[cpu.PC] = 0x17     # SLO ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x41 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[effectiveAddr.uint16] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0E02          # PC advanced by 2
      cpu.cycles == 6           # SLO ZeroPage,X takes 6 cycles

  test "SLO ZeroPage,X - Sets Carry Flag":
    # Setup: SLO $50,X where X=0x05 (17 50)
    # Effective address = $50 + $05 = $55
    # Value M at $0055 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1.
    # 2. Write $02 back to $0055.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$0055] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 6
    cpu.PC = 0x0F00
    cpu.X = 0x05
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpBaseAddr = 0x50'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF

    mem.mem[cpu.PC] = 0x17     # SLO ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x81 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[effectiveAddr.uint16] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x0F02          # PC advanced by 2
      cpu.cycles == 6           # Cycles correct

  test "SLO ZeroPage,X - Sets Zero Flag":
    # Setup: SLO $60,X where X=0x08 (17 60)
    # Effective address = $60 + $08 = $68
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
    # Cycles = Cycles + 6
    cpu.PC = 0x1000
    cpu.X = 0x08
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially, clear C
    cpu.cycles = 0
    let zpBaseAddr = 0x60'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF

    mem.mem[cpu.PC] = 0x17     # SLO ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x80 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[effectiveAddr.uint16] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      cpu.C == true             # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x1002          # PC advanced by 2
      cpu.cycles == 6           # Cycles correct

  test "SLO ZeroPage,X - Sets Negative Flag":
    # Setup: SLO $70,X where X=0x0A (17 70)
    # Effective address = $70 + $0A = $7A
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
    # Cycles = Cycles + 6
    cpu.PC = 0x1100
    cpu.X = 0x0A
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0
    let zpBaseAddr = 0x70'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF

    mem.mem[cpu.PC] = 0x17     # SLO ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x40 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81             # Accumulator updated (01 | 80 = 81)
      mem.mem[effectiveAddr.uint16] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1102          # PC advanced by 2
      cpu.cycles == 6           # Cycles correct

  test "SLO ZeroPage,X - Zero Page Wrap-around":
    # Setup: SLO $F0,X where X=$15 (17 F0)
    # Effective address = $F0 + $15 = $105 -> wraps to $05
    # Value M at $0005 is $C0 (11000000)
    # Initial A = $03 (00000011)
    #
    # Action:
    # 1. ASL on M: $C0 << 1 = $80 (10000000). Carry = 1.
    # 2. Write $80 back to $0005.
    # 3. ORA: A = A | shifted M = $03 | $80 = $83 (10000011)
    #
    # Expected State:
    # A = $83
    # Memory[$0005] = $80
    # Flags: N=1, Z=0, C=1
    # PC = PC + 2
    # Cycles = Cycles + 6
    cpu.PC = 0x1200
    cpu.X = 0x15
    cpu.A = 0x03
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0
    let zpBaseAddr = 0xF0'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # Should be 0x05

    mem.mem[cpu.PC] = 0x17     # SLO ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0xC0 # Initial value M at $0005

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      effectiveAddr == 0x05     # Verify wrap-around calculation
      cpu.A == 0x83             # Accumulator updated (03 | 80 = 83)
      mem.mem[effectiveAddr.uint16] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      cpu.C == true             # Carry flag set
      cpu.PC == 0x1202          # PC advanced by 2
      cpu.cycles == 6           # Cycles correct


  # --- Tests for Opcode 0x1B: SLO Absolute,Y ---

  test "SLO Absolute,Y - Basic Operation, No Carry, No Page Cross":
    # Setup: SLO $C050,Y where Y=0x05 (1B 50 C0)
    # Base address = $C050
    # Effective address = $C050 + Y = $C050 + $05 = $C055
    # Initial value M at $C055 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0.
    # 2. Write $82 back to $C055.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$C055] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1300
    cpu.Y = 0x05
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let baseAddr: uint16 = 0xC050
    let effectiveAddr = baseAddr + cpu.Y.uint16

    mem.mem[cpu.PC] = 0x1B       # SLO Absolute,Y
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x41 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[effectiveAddr] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x1303          # PC advanced by 3
      cpu.cycles == 7           # SLO Absolute,Y takes 7 cycles

  test "SLO Absolute,Y - Sets Carry Flag, No Page Cross":
    # Setup: SLO $D450,Y where Y=0x02 (1B 50 D4)
    # Base address = $D450
    # Effective address = $D450 + Y = $D450 + $02 = $D452
    # Initial value M at $D452 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1.
    # 2. Write $02 back to $D452.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$D452] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1400
    cpu.Y = 0x02
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xD450
    let effectiveAddr = baseAddr + cpu.Y.uint16

    mem.mem[cpu.PC] = 0x1B       # SLO Absolute,Y
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x81 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[effectiveAddr] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x1403          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,Y - Sets Zero Flag, No Page Cross":
    # Setup: SLO $E780,Y where Y=0x01 (1B 80 E7)
    # Base address = $E780
    # Effective address = $E780 + Y = $E780 + $01 = $E781
    # Initial value M at $E781 is $80 (10000000) -> ASL -> $00, C=1
    # Initial A = $00 (00000000)
    #
    # Action:
    # 1. ASL on M: $80 << 1 = $00. Carry = 1.
    # 2. Write $00 back to $E781.
    # 3. ORA: A = A | shifted M = $00 | $00 = $00
    #
    # Expected State:
    # A = $00
    # Memory[$E781] = $00
    # Flags: N=0, Z=1, C=1
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1500
    cpu.Y = 0x01
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially, clear C
    cpu.cycles = 0
    let baseAddr: uint16 = 0xE780
    let effectiveAddr = baseAddr + cpu.Y.uint16

    mem.mem[cpu.PC] = 0x1B       # SLO Absolute,Y
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x80 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[effectiveAddr] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      cpu.C == true             # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x1503          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,Y - Sets Negative Flag, No Page Cross":
    # Setup: SLO $F010,Y where Y=0x03 (1B 10 F0)
    # Base address = $F010
    # Effective address = $F010 + Y = $F010 + $03 = $F013
    # Initial value M at $F013 is $40 (01000000) -> ASL -> $80, C=0
    # Initial A = $01 (00000001)
    #
    # Action:
    # 1. ASL on M: $40 << 1 = $80. Carry = 0.
    # 2. Write $80 back to $F013.
    # 3. ORA: A = A | shifted M = $01 | $80 = $81
    #
    # Expected State:
    # A = $81
    # Memory[$F013] = $80
    # Flags: N=1, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1600
    cpu.Y = 0x03
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xF010
    let effectiveAddr = baseAddr + cpu.Y.uint16

    mem.mem[cpu.PC] = 0x1B       # SLO Absolute,Y
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x40 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81             # Accumulator updated (01 | 80 = 81)
      mem.mem[effectiveAddr] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1603          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,Y - Page Crossing":
    # Setup: SLO $C180,Y where Y=0x85 (1B 80 C1)
    # Base address = $C180
    # Effective address = $C180 + Y = $C180 + $85 = $C205 (crosses page boundary)
    # Initial value M at $C205 is $01 (00000001)
    # Initial A = $02 (00000010)
    #
    # Action:
    # 1. ASL on M: $01 << 1 = $02. Carry = 0.
    # 2. Write $02 back to $C205.
    # 3. ORA: A = A | shifted M = $02 | $02 = $02
    #
    # Expected State:
    # A = $02
    # Memory[$C205] = $02
    # Flags: N=0, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7 (Fixed for SLO, even with page cross)
    cpu.PC = 0x1700
    cpu.Y = 0x85
    cpu.A = 0x02
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x80'u8 or 0x02'u8) # Set N,Z,C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xC180
    let effectiveAddr = baseAddr + cpu.Y.uint16

    mem.mem[cpu.PC] = 0x1B       # SLO Absolute,Y
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x01 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x02             # Accumulator updated (02 | 02 = 02)
      mem.mem[effectiveAddr] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      not cpu.N                 # Negative flag clear
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1703          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct (fixed at 7)


  # --- Tests for Opcode 0x1F: SLO Absolute,X ---

  test "SLO Absolute,X - Basic Operation, No Carry, No Page Cross":
    # Setup: SLO $C050,X where X=0x05 (1F 50 C0)
    # Base address = $C050
    # Effective address = $C050 + X = $C050 + $05 = $C055
    # Initial value M at $C055 is $41 (01000001)
    # Initial A = $12 (00010010)
    #
    # Action:
    # 1. ASL on M: $41 << 1 = $82 (10000010). Carry = 0.
    # 2. Write $82 back to $C055.
    # 3. ORA: A = A | shifted M = $12 | $82 = $92 (10010010)
    #
    # Expected State:
    # A = $92
    # Memory[$C055] = $82
    # Flags: N=1, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1800
    cpu.X = 0x05
    cpu.A = 0x12
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let baseAddr: uint16 = 0xC050
    let effectiveAddr = baseAddr + cpu.X.uint16

    mem.mem[cpu.PC] = 0x1F       # SLO Absolute,X
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x41 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x92             # Accumulator updated (12 | 82 = 92)
      mem.mem[effectiveAddr] == 0x82   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (92 != 0)
      cpu.N == true             # Negative flag set (bit 7 of 92 is 1)
      not cpu.C                 # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x1803          # PC advanced by 3
      cpu.cycles == 7           # SLO Absolute,X takes 7 cycles

  test "SLO Absolute,X - Sets Carry Flag, No Page Cross":
    # Setup: SLO $D450,X where X=0x02 (1F 50 D4)
    # Base address = $D450
    # Effective address = $D450 + X = $D450 + $02 = $D452
    # Initial value M at $D452 is $81 (10000001)
    # Initial A = $0F (00001111)
    #
    # Action:
    # 1. ASL on M: $81 << 1 = $02 (00000010). Carry = 1.
    # 2. Write $02 back to $D452.
    # 3. ORA: A = A | shifted M = $0F | $02 = $0F (00001111)
    #
    # Expected State:
    # A = $0F
    # Memory[$D452] = $02
    # Flags: N=0, Z=0, C=1
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1900
    cpu.X = 0x02
    cpu.A = 0x0F
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xD450
    let effectiveAddr = baseAddr + cpu.X.uint16

    mem.mem[cpu.PC] = 0x1F       # SLO Absolute,X
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x81 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x0F             # Accumulator updated (0F | 02 = 0F)
      mem.mem[effectiveAddr] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear (0F != 0)
      not cpu.N                 # Negative flag clear (bit 7 of 0F is 0)
      cpu.C == true             # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x1903          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,X - Sets Zero Flag, No Page Cross":
    # Setup: SLO $E780,X where X=0x01 (1F 80 E7)
    # Base address = $E780
    # Effective address = $E780 + X = $E780 + $01 = $E781
    # Initial value M at $E781 is $80 (10000000) -> ASL -> $00, C=1
    # Initial A = $00 (00000000)
    #
    # Action:
    # 1. ASL on M: $80 << 1 = $00. Carry = 1.
    # 2. Write $00 back to $E781.
    # 3. ORA: A = A | shifted M = $00 | $00 = $00
    #
    # Expected State:
    # A = $00
    # Memory[$E781] = $00
    # Flags: N=0, Z=1, C=1
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1A00
    cpu.X = 0x01
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially, clear C
    cpu.cycles = 0
    let baseAddr: uint16 = 0xE780
    let effectiveAddr = baseAddr + cpu.X.uint16

    mem.mem[cpu.PC] = 0x1F       # SLO Absolute,X
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x80 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00             # Accumulator is zero
      mem.mem[effectiveAddr] == 0x00   # Memory updated with shifted value
      cpu.Z == true             # Zero flag set
      not cpu.N                 # Negative flag clear
      cpu.C == true             # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x1A03          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,X - Sets Negative Flag, No Page Cross":
    # Setup: SLO $F010,X where X=0x03 (1F 10 F0)
    # Base address = $F010
    # Effective address = $F010 + X = $F010 + $03 = $F013
    # Initial value M at $F013 is $40 (01000000) -> ASL -> $80, C=0
    # Initial A = $01 (00000001)
    #
    # Action:
    # 1. ASL on M: $40 << 1 = $80. Carry = 0.
    # 2. Write $80 back to $F013.
    # 3. ORA: A = A | shifted M = $01 | $80 = $81
    #
    # Expected State:
    # A = $81
    # Memory[$F013] = $80
    # Flags: N=1, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7
    cpu.PC = 0x1B00
    cpu.X = 0x03
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xF010
    let effectiveAddr = baseAddr + cpu.X.uint16

    mem.mem[cpu.PC] = 0x1F       # SLO Absolute,X
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x40 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81             # Accumulator updated (01 | 80 = 81)
      mem.mem[effectiveAddr] == 0x80   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      cpu.N == true             # Negative flag set
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1B03          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct

  test "SLO Absolute,X - Page Crossing":
    # Setup: SLO $C180,X where X=0x85 (1F 80 C1)
    # Base address = $C180
    # Effective address = $C180 + X = $C180 + $85 = $C205 (crosses page boundary)
    # Initial value M at $C205 is $01 (00000001)
    # Initial A = $02 (00000010)
    #
    # Action:
    # 1. ASL on M: $01 << 1 = $02. Carry = 0.
    # 2. Write $02 back to $C205.
    # 3. ORA: A = A | shifted M = $02 | $02 = $02
    #
    # Expected State:
    # A = $02
    # Memory[$C205] = $02
    # Flags: N=0, Z=0, C=0
    # PC = PC + 3
    # Cycles = Cycles + 7 (Fixed for SLO, even with page cross)
    cpu.PC = 0x1C00
    cpu.X = 0x85
    cpu.A = 0x02
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x80'u8 or 0x02'u8) # Set N,Z,C initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xC180
    let effectiveAddr = baseAddr + cpu.X.uint16

    mem.mem[cpu.PC] = 0x1F       # SLO Absolute,X
    mem.mem[cpu.PC + 1] = uint8(baseAddr and 0xFF) # Low byte
    mem.mem[cpu.PC + 2] = uint8(baseAddr shr 8)    # High byte
    mem.mem[effectiveAddr] = 0x01 # Initial value M

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x02             # Accumulator updated (02 | 02 = 02)
      mem.mem[effectiveAddr] == 0x02   # Memory updated with shifted value
      not cpu.Z                 # Zero flag clear
      not cpu.N                 # Negative flag clear
      not cpu.C                 # Carry flag clear
      cpu.PC == 0x1C03          # PC advanced by 3
      cpu.cycles == 7           # Cycles correct (fixed at 7)

