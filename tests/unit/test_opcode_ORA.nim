import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "ORA Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  # --- Tests for Opcode 0x09: ORA Immediate ---

  test "ORA Immediate - Basic OR, Positive Result":
    # Setup ORA #$0F (09 0F)
    # Initial A = $5A (01011010)
    # Expected A = $5A | $0F = $5F (01011111)
    cpu.PC = 0x0300
    cpu.A = 0x5A
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x02'u8) # Set N and Z initially to ensure they are cleared
    cpu.cycles = 0

    mem.mem[0x0300] = 0x09  # ORA immediate
    mem.mem[0x0301] = 0x0F  # Value to OR

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet, checks below will fail if it were.

    check:
      cpu.A == 0x5F        # Accumulator has ORed value
      not cpu.Z            # Zero flag clear (0x5F != 0)
      not cpu.N            # Negative flag clear (bit 7 = 0)
      cpu.PC == 0x0302     # PC advanced past instruction + operand
      cpu.cycles == 2      # ORA immediate takes 2 cycles

  test "ORA Immediate - Sets Zero Flag":
    # Setup ORA #$00 (09 00)
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x0300
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0

    mem.mem[0x0300] = 0x09  # ORA immediate
    mem.mem[0x0301] = 0x00  # Value to OR

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet.

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x0302
      cpu.cycles == 2

  test "ORA Immediate - Sets Negative Flag":
    # Setup ORA #$80 (09 80)
    # Initial A = $01
    # Expected A = $01 | $80 = $81
    cpu.PC = 0x0300
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially
    cpu.cycles = 0

    mem.mem[0x0300] = 0x09  # ORA immediate
    mem.mem[0x0301] = 0x80  # Value to OR (negative)

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet.

    check:
      cpu.A == 0x81        # Accumulator has ORed value
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0302
      cpu.cycles == 2


  # --- Tests for Opcode 0x01: ORA (Indirect,X) ---

  test "ORA (Indirect,X) - Basic Operation":
    # Setup: ORA ($40,X) where X=0x04
    # Zero page address = $40 + $04 = $44
    # Effective address stored at $0044/$0045 is $1234
    # Value at $1234 is $55
    # Initial A = $AA
    # Expected A = $AA | $55 = $FF
    cpu.PC = 0x0200
    cpu.X = 0x04
    cpu.A = 0xAA
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0200] = 0x01  # ORA (Indirect,X)
    mem.mem[0x0201] = 0x40  # Zero page base address

    # Setup the indirect address lookup in zero page
    mem.mem[0x0044] = 0x34  # Low byte of effective address ($1234)
    mem.mem[0x0045] = 0x12  # High byte of effective address ($1234)

    # Setup the value at the effective address
    mem.mem[0x1234] = 0x55

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)

    check:
      cpu.A == 0xFF        # Accumulator updated (AA | 55 = FF)
      not cpu.Z            # Zero flag clear (FF != 0)
      cpu.N == true        # Negative flag set (bit 7 of FF is 1)
      cpu.PC == 0x0202     # PC advanced by 2
      cpu.cycles == 6      # ORA (Indirect,X) takes 6 cycles

  test "ORA (Indirect,X) - Zero Page Wrap-around":
    # Setup: ORA ($FE,X) where X=0x03
    # Zero page address = $FE + $03 = $101 -> wraps to $01
    # Effective address stored at $0001/$0002 is $BEEF
    # Value at $BEEF is $0F
    # Initial A = $F0
    # Expected A = $F0 | $0F = $FF
    cpu.PC = 0x0200
    cpu.X = 0x03
    cpu.A = 0xF0
    cpu.setFlags(0x20'u8)
    cpu.cycles = 0

    mem.mem[0x0200] = 0x01  # ORA (Indirect,X)
    mem.mem[0x0201] = 0xFE  # Zero page base address

    # Setup the indirect address lookup in zero page (with wrap)
    mem.mem[0x0001] = 0xEF  # Low byte of effective address ($BEEF)
    mem.mem[0x0002] = 0xBE  # High byte of effective address ($BEEF)

    # Setup the value at the effective address
    mem.mem[0xBEEF] = 0x0F

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)

    check:
      cpu.A == 0xFF        # Accumulator updated (F0 | 0F = FF)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0202     # PC advanced by 2
      cpu.cycles == 6      # Cycles correct

  test "ORA (Indirect,X) - Sets Zero Flag":
    # Setup: ORA ($10,X) where X=0x02 -> ZP addr $12
    # Effective address at $0012/$0013 is $C000
    # Value at $C000 is $00
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x0200
    cpu.X = 0x02
    cpu.A = 0x00
    cpu.setFlags(0x20'u8)
    cpu.N = true # Set N initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0200] = 0x01  # ORA (Indirect,X)
    mem.mem[0x0201] = 0x10  # Zero page base address

    mem.mem[0x0012] = 0x00  # Low byte of $C000
    mem.mem[0x0013] = 0xC0  # High byte of $C000
    mem.mem[0xC000] = 0x00  # Value to OR

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x0202
      cpu.cycles == 6

  test "ORA (Indirect,X) - Clears Negative Flag":
    # Setup: ORA ($20,X) where X=0x05 -> ZP addr $25
    # Effective address at $0025/$0026 is $D000
    # Value at $D000 is $0F
    # Initial A = $70 (N flag clear)
    # Expected A = $70 | $0F = $7F (N flag still clear)
    cpu.PC = 0x0200
    cpu.X = 0x05
    cpu.A = 0x70
    cpu.setFlags(0x20'u8)
    cpu.N = true # Set N initially to ensure it gets cleared if result is not negative
    cpu.cycles = 0

    mem.mem[0x0200] = 0x01  # ORA (Indirect,X)
    mem.mem[0x0201] = 0x20  # Zero page base address

    mem.mem[0x0025] = 0x00  # Low byte of $D000
    mem.mem[0x0026] = 0xD0  # High byte of $D000
    mem.mem[0xD000] = 0x0F  # Value to OR

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)

    check:
      cpu.A == 0x7F        # Accumulator updated (70 | 0F = 7F)
      not cpu.Z            # Zero flag clear
      not cpu.N            # Negative flag clear (bit 7 is 0)
      cpu.PC == 0x0202
      cpu.cycles == 6

  # --- Tests for Opcode 0x0D: ORA Absolute ---

  test "ORA Absolute - Basic OR, Positive Result":
    # Setup: ORA $1234 (0D 34 12)
    # Value at $1234 = $0F
    # Initial A = $50 (01010000)
    # Expected A = $50 | $0F = $5F (01011111)
    cpu.PC = 0x0600
    cpu.A = 0x50
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x02'u8) # Set N and Z initially to ensure they are cleared
    cpu.cycles = 0

    mem.mem[0x0600] = 0x0D  # ORA Absolute
    mem.mem[0x0601] = 0x34  # Low byte of address
    mem.mem[0x0602] = 0x12  # High byte of address
    mem.mem[0x1234] = 0x0F  # Value at target address

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet.

    check:
      cpu.A == 0x5F        # Accumulator has ORed value
      not cpu.Z            # Zero flag clear (0x5F != 0)
      not cpu.N            # Negative flag clear (bit 7 = 0)
      cpu.PC == 0x0603     # PC advanced past instruction + operands (3 bytes)
      cpu.cycles == 4      # ORA Absolute takes 4 cycles

  test "ORA Absolute - Sets Zero Flag":
    # Setup: ORA $C000 (0D 00 C0)
    # Value at $C000 = $00
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x0700
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0

    mem.mem[0x0700] = 0x0D  # ORA Absolute
    mem.mem[0x0701] = 0x00  # Low byte of address
    mem.mem[0x0702] = 0xC0  # High byte of address
    mem.mem[0xC000] = 0x00  # Value at target address

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet.

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x0703     # PC advanced by 3
      cpu.cycles == 4      # Cycles correct

  test "ORA Absolute - Sets Negative Flag":
    # Setup: ORA $BEEF (0D EF BE)
    # Value at $BEEF = $80
    # Initial A = $01
    # Expected A = $01 | $80 = $81
    cpu.PC = 0x0800
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially
    cpu.cycles = 0

    mem.mem[0x0800] = 0x0D  # ORA Absolute
    mem.mem[0x0801] = 0xEF  # Low byte of address
    mem.mem[0x0802] = 0xBE  # High byte of address
    mem.mem[0xBEEF] = 0x80  # Value at target address (negative)

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard # Handler not implemented yet.

    check:
      cpu.A == 0x81        # Accumulator has ORed value
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0803     # PC advanced by 3
      cpu.cycles == 4      # Cycles correct

  # --- Tests for Opcode 0x05: ORA ZeroPage ---

  test "ORA ZeroPage - Basic Operation":
    # Setup: ORA $30
    # Value at $0030 is $55
    # Initial A = $AA
    # Expected A = $AA | $55 = $FF
    cpu.PC = 0x0900
    cpu.A = 0xAA
    cpu.setFlags(0x20'u8) # Clear N, Z initially
    cpu.cycles = 0

    mem.mem[0x0900] = 0x05  # ORA ZeroPage
    mem.mem[0x0901] = 0x30  # Zero page address operand

    # Setup the value in zero page
    mem.mem[0x0030] = 0x55

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0xFF        # Accumulator updated (AA | 55 = FF)
      not cpu.Z            # Zero flag clear (FF != 0)
      cpu.N == true        # Negative flag set (bit 7 of FF is 1)
      cpu.PC == 0x0902     # PC advanced by 2
      cpu.cycles == 3      # ORA ZeroPage takes 3 cycles

  test "ORA ZeroPage - Sets Zero Flag":
    # Setup: ORA $31
    # Value at $0031 is $00
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x0910
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0910] = 0x05  # ORA ZeroPage
    mem.mem[0x0911] = 0x31  # Zero page address operand

    # Setup the value in zero page
    mem.mem[0x0031] = 0x00

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x0912     # PC advanced by 2
      cpu.cycles == 3      # Cycles correct

  test "ORA ZeroPage - Sets Negative Flag":
    # Setup: ORA $32
    # Value at $0032 is $80
    # Initial A = $01
    # Expected A = $01 | $80 = $81
    cpu.PC = 0x0920
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0920] = 0x05  # ORA ZeroPage
    mem.mem[0x0921] = 0x32  # Zero page address operand

    # Setup the value in zero page
    mem.mem[0x0032] = 0x80

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x81        # Accumulator updated (01 | 80 = 81)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0922     # PC advanced by 2
      cpu.cycles == 3      # Cycles correct

  test "ORA ZeroPage - Clears Negative Flag":
    # Setup: ORA $33
    # Value at $0033 is $0F
    # Initial A = $70 (N flag clear)
    # Expected A = $70 | $0F = $7F (N flag still clear)
    cpu.PC = 0x0930
    cpu.A = 0x70
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x0930] = 0x05  # ORA ZeroPage
    mem.mem[0x0931] = 0x33  # Zero page address operand

    # Setup the value in zero page
    mem.mem[0x0033] = 0x0F

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()

    check:
      cpu.A == 0x7F        # Accumulator updated (70 | 0F = 7F)
      not cpu.Z            # Zero flag clear
      not cpu.N            # Negative flag clear (bit 7 is 0)
      cpu.PC == 0x0932     # PC advanced by 2
      cpu.cycles == 3      # Cycles correct

  # --- Tests for Opcode 0x11: ORA (Indirect),Y ---

  test "ORA (Indirect),Y - Basic Operation, No Page Cross":
    # Setup: ORA ($40),Y where Y=0x04
    # Zero page address operand = $40
    # Base address stored at $0040/$0041 is $1230
    # Effective address = $1230 + Y = $1230 + $04 = $1234 (no page cross)
    # Value at $1234 is $55
    # Initial A = $AA
    # Expected A = $AA | $55 = $FF
    cpu.PC = 0x0A00
    cpu.Y = 0x04
    cpu.A = 0xAA
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0A00] = 0x11  # ORA (Indirect),Y
    mem.mem[0x0A01] = 0x40  # Zero page address operand

    # Setup the indirect base address lookup in zero page
    mem.mem[0x0040] = 0x30  # Low byte of base address ($1230)
    mem.mem[0x0041] = 0x12  # High byte of base address ($1230)

    # Setup the value at the effective address
    mem.mem[0x1234] = 0x55

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail() # Expect failure

    check:
      cpu.A == 0xFF        # Accumulator updated (AA | 55 = FF)
      not cpu.Z            # Zero flag clear (FF != 0)
      cpu.N == true        # Negative flag set (bit 7 of FF is 1)
      cpu.PC == 0x0A02     # PC advanced by 2
      cpu.cycles == 5      # ORA (Indirect),Y takes 5 cycles (no page cross)

  test "ORA (Indirect),Y - Page Cross":
    # Setup: ORA ($50),Y where Y=0x10
    # Zero page address operand = $50
    # Base address stored at $0050/$0051 is $12F8
    # Effective address = $12F8 + Y = $12F8 + $10 = $1308 (page cross)
    # Value at $1308 is $0F
    # Initial A = $F0
    # Expected A = $F0 | $0F = $FF
    cpu.PC = 0x0B00
    cpu.Y = 0x10
    cpu.A = 0xF0
    cpu.setFlags(0x20'u8) # Clear flags initially
    cpu.cycles = 0

    mem.mem[0x0B00] = 0x11  # ORA (Indirect),Y
    mem.mem[0x0B01] = 0x50  # Zero page address operand

    # Setup the indirect base address lookup in zero page
    mem.mem[0x0050] = 0xF8  # Low byte of base address ($12F8)
    mem.mem[0x0051] = 0x12  # High byte of base address ($12F8)

    # Setup the value at the effective address
    mem.mem[0x1308] = 0x0F

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail() # Expect failure

    check:
      cpu.A == 0xFF        # Accumulator updated (F0 | 0F = FF)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0B02     # PC advanced by 2
      cpu.cycles == 6      # ORA (Indirect),Y takes 5 + 1 = 6 cycles (page cross)

  test "ORA (Indirect),Y - Sets Zero Flag":
    # Setup: ORA ($60),Y where Y=0x05
    # Zero page address operand = $60
    # Base address stored at $0060/$0061 is $C010
    # Effective address = $C010 + Y = $C010 + $05 = $C015
    # Value at $C015 is $00
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x0C00
    cpu.Y = 0x05
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0

    mem.mem[0x0C00] = 0x11  # ORA (Indirect),Y
    mem.mem[0x0C01] = 0x60  # Zero page address operand

    # Setup the indirect base address lookup in zero page
    mem.mem[0x0060] = 0x10  # Low byte of base address ($C010)
    mem.mem[0x0061] = 0xC0  # High byte of base address ($C010)

    # Setup the value at the effective address
    mem.mem[0xC015] = 0x00

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail() # Expect failure

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x0C02     # PC advanced by 2
      cpu.cycles == 5      # No page cross

  test "ORA (Indirect),Y - Sets Negative Flag":
    # Setup: ORA ($70),Y where Y=0x0A
    # Zero page address operand = $70
    # Base address stored at $0070/$0071 is $D020
    # Effective address = $D020 + Y = $D020 + $0A = $D02A
    # Value at $D02A is $80
    # Initial A = $01
    # Expected A = $01 | $80 = $81
    cpu.PC = 0x0D00
    cpu.Y = 0x0A
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially
    cpu.cycles = 0

    mem.mem[0x0D00] = 0x11  # ORA (Indirect),Y
    mem.mem[0x0D01] = 0x70  # Zero page address operand

    # Setup the indirect base address lookup in zero page
    mem.mem[0x0070] = 0x20  # Low byte of base address ($D020)
    mem.mem[0x0071] = 0xD0  # High byte of base address ($D020)

    # Setup the value at the effective address
    mem.mem[0xD02A] = 0x80

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail() # Expect failure

    check:
      cpu.A == 0x81        # Accumulator updated (01 | 80 = 81)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0D02     # PC advanced by 2
      cpu.cycles == 5      # No page cross

  # --- Tests for Opcode 0x15: ORA ZeroPage,X ---

  test "ORA ZeroPage,X - Basic Operation":
    # Setup: ORA $30,X where X=$05
    # Effective address = $30 + $05 = $35
    # Value at $0035 is $55
    # Initial A = $AA
    # Expected A = $AA | $55 = $FF
    cpu.PC = 0x0E00
    cpu.X = 0x05
    cpu.A = 0xAA
    cpu.setFlags(0x20'u8) # Clear N, Z initially
    cpu.cycles = 0

    mem.mem[0x0E00] = 0x15  # ORA ZeroPage,X
    mem.mem[0x0E01] = 0x30  # Zero page base address operand

    # Setup the value in zero page at the effective address
    mem.mem[0x0035] = 0x55

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard

    check:
      cpu.A == 0xFF        # Accumulator updated (AA | 55 = FF)
      not cpu.Z            # Zero flag clear (FF != 0)
      cpu.N == true        # Negative flag set (bit 7 of FF is 1)
      cpu.PC == 0x0E02     # PC advanced by 2
      cpu.cycles == 4      # ORA ZeroPage,X takes 4 cycles

  test "ORA ZeroPage,X - Zero Page Wrap-around":
    # Setup: ORA $F0,X where X=$15
    # Effective address = $F0 + $15 = $105 -> wraps to $05
    # Value at $0005 is $0F
    # Initial A = $F0
    # Expected A = $F0 | $0F = $FF
    cpu.PC = 0x0F00
    cpu.X = 0x15
    cpu.A = 0xF0
    cpu.setFlags(0x20'u8) # Clear N, Z initially
    cpu.cycles = 0

    mem.mem[0x0F00] = 0x15  # ORA ZeroPage,X
    mem.mem[0x0F01] = 0xF0  # Zero page base address operand

    # Setup the value in zero page at the wrapped effective address
    mem.mem[0x0005] = 0x0F

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard

    check:
      cpu.A == 0xFF        # Accumulator updated (F0 | 0F = FF)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x0F02     # PC advanced by 2
      cpu.cycles == 4      # Cycles correct

  test "ORA ZeroPage,X - Sets Zero Flag":
    # Setup: ORA $40,X where X=$02
    # Effective address = $40 + $02 = $42
    # Value at $0042 is $00
    # Initial A = $00
    # Expected A = $00 | $00 = $00
    cpu.PC = 0x1000
    cpu.X = 0x02
    cpu.A = 0x00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x1000] = 0x15  # ORA ZeroPage,X
    mem.mem[0x1001] = 0x40  # Zero page base address operand

    # Setup the value in zero page
    mem.mem[0x0042] = 0x00

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard

    check:
      cpu.A == 0x00        # Accumulator is zero
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear
      cpu.PC == 0x1002     # PC advanced by 2
      cpu.cycles == 4      # Cycles correct

  test "ORA ZeroPage,X - Sets Negative Flag":
    # Setup: ORA $50,X where X=$03
    # Effective address = $50 + $03 = $53
    # Value at $0053 is $80
    # Initial A = $01
    # Expected A = $01 | $80 = $81
    cpu.PC = 0x1100
    cpu.X = 0x03
    cpu.A = 0x01
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x1100] = 0x15  # ORA ZeroPage,X
    mem.mem[0x1101] = 0x50  # Zero page base address operand

    # Setup the value in zero page
    mem.mem[0x0053] = 0x80

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard

    check:
      cpu.A == 0x81        # Accumulator updated (01 | 80 = 81)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set
      cpu.PC == 0x1102     # PC advanced by 2
      cpu.cycles == 4      # Cycles correct

  test "ORA ZeroPage,X - Clears Negative Flag":
    # Setup: ORA $60,X where X=$04
    # Effective address = $60 + $04 = $64
    # Value at $0064 is $0F
    # Initial A = $70 (N flag clear)
    # Expected A = $70 | $0F = $7F (N flag still clear)
    cpu.PC = 0x1200
    cpu.X = 0x04
    cpu.A = 0x70
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared
    cpu.cycles = 0

    mem.mem[0x1200] = 0x15  # ORA ZeroPage,X
    mem.mem[0x1201] = 0x60  # Zero page base address operand

    # Setup the value in zero page
    mem.mem[0x0064] = 0x0F

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      discard

    check:
      cpu.A == 0x7F        # Accumulator updated (70 | 0F = 7F)
      not cpu.Z            # Zero flag clear
      not cpu.N            # Negative flag clear (bit 7 is 0)
      cpu.PC == 0x1202     # PC advanced by 2
      cpu.cycles == 4      # Cycles correct

