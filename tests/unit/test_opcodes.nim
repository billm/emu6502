import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags

suite "Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "Unimplemented opcode raises correct exception":
    # First, clear the opcode table entry for 0xFF (likely unimplemented)
    opcodeTable[0xFF].handler = nil
    
    # Set PC to point to our test opcode
    cpu.PC = 0x300
    mem.mem[0x300] = 0xFF

    expect UnimplementedOpcodeError:
      cpu.execute()

    try:
      cpu.execute()
    except UnimplementedOpcodeError as e:
      check:
        e.opcode == 0xFF
        e.pc == 0x300
        e.msg == "Unimplemented opcode: FF @ PC: 0x0300"

  test "BRK instruction works correctly":
    # Set up IRQ vector at 0xFFFE/FFFF to point to handler
    mem.mem[0xFFFE] = 0x00'u8  # Low byte of IRQ vector
    mem.mem[0xFFFF] = 0x80'u8  # IRQ handler at 0x8000
    
    # Initial CPU state
    cpu.PC = 0x0300
    cpu.SP = 0xFF'u8
    cpu.I = false  # Ensure interrupt disable starts cleared
    cpu.B = false  # Break flag should start cleared
    cpu.setFlags(0b00100000'u8)  # Only unused flag set
    cpu.cycles = 0
    
    # Set up BRK instruction
    mem.mem[0x0300] = 0x00'u8  # BRK opcode
    
    # Execute BRK 
    let info = opcodeTable[mem.mem[cpu.PC].uint8]
    info.handler(cpu)
    
    # Stack should contain PC+2 (high byte, low byte) and flags with B set
    let finalSP = cpu.SP.uint16 # Should be 0xFC
    let stackedFlags = mem.mem[0x0100 + finalSP + 1] # Status at 0x01FD
    let stackedPCLow = mem.mem[0x0100 + finalSP + 2] # PCL at 0x01FE
    let stackedPCHigh = mem.mem[0x0100 + finalSP + 3] # PCH at 0x01FF
    
    check:
      # PC + 2 was pushed correctly
      stackedPCHigh == 0x03'u8  # High byte of 0x0302
      stackedPCLow == 0x02'u8   # Low byte of 0x0302
      
      # Status was pushed with B set
      (stackedFlags and 0b00110000'u8) == 0b00110000'u8  # Both B and U set
      (stackedFlags and 0b00001111'u8) == 0b00000000'u8  # Other flags clear
      
      # CPU state after BRK
      cpu.I == true           # Interrupt disable set
      cpu.B == false         # B not set in actual status
      cpu.PC == 0x8000       # Loaded from IRQ vector
      cpu.SP == 0xFF'u8 - 3'u8  # Pushed 3 bytes
      cpu.cycles == 7'u16    # BRK takes 7 cycles

  # As opcode handlers are implemented, add specific tests for each one.
  
  test "LDA immediate mode sets accumulator and flags - positive value":
    # Setup LDA #$42 (A9 42)
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x42  # Value to load
    cpu.cycles = 0  # Reset cycles
    
    # Execute just one instruction
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
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
    info.handler(cpu)
    
    check cpu.Z == true    # Zero flag should be set

  test "LDA immediate mode sets negative flag":
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x80  # Load negative value (bit 7 set)
    
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
    check:
      cpu.N == true        # Negative flag should be set
      not cpu.Z            # Zero flag should be clear
  

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

  # More opcode tests will be added here as they are implemented,
  # following Test-Driven Development principles

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
      fail() # Fail explicitly if not implemented

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



  # --- Tests for Opcode 0x04: NOP ZeroPage (Unofficial) ---

  test "NOP ZeroPage (0x04) - No Operation":
    # Setup: NOP $5A (04 5A)
    # Action: Reads from $005A but does nothing with the value.
    # Expected: PC+=2, Cycles+=3. A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0xBB
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100) # Set some flags initially
    cpu.cycles = 10

    mem.mem[0x0800] = 0x04  # NOP ZeroPage opcode
    mem.mem[0x0801] = 0x5A  # Zero page address operand
    mem.mem[0x005A] = 0xDD  # Value at the zero page address (should be read but ignored)

    let initialA = cpu.A
    let initialX = cpu.X
    let initialY = cpu.Y
    let initialSP = cpu.SP
    let initialFlags = cpu.flags()
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail() # Fail explicitly if not implemented

    check:
      # State unchanged
      cpu.A == initialA
      cpu.X == initialX
      cpu.Y == initialY
      cpu.SP == initialSP
      cpu.flags() == initialFlags

      # PC and Cycles updated
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 3

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

# --- Tests for Opcode 0x06: ASL ZeroPage ---

  test "ASL ZeroPage - Basic Shift, No Carry":
    # Setup: ASL $42 (06 42)
    # Value at $0042 is $41 (01000001)
    # Expected: Memory[$0042] = $82 (10000010), C=0, Z=0, N=1
    cpu.PC = 0x0900
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let zpAddr = 0x42'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x41 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x82 # Memory updated with shifted value
      not cpu.Z                      # Zero flag clear (82 != 0)
      cpu.N == true                  # Negative flag set (bit 7 of 82 is 1)
      not cpu.C                      # Carry flag clear (original bit 7 of 41 was 0)
      cpu.PC == 0x0902               # PC advanced by 2
      cpu.cycles == 5                # ASL ZeroPage takes 5 cycles
  
  test "ASL ZeroPage - Sets Carry Flag":
    # Setup: ASL $55 (06 55)
    # Value at $0055 is $81 (10000001)
    # Expected: Memory[$0055] = $02 (00000010), C=1, Z=0, N=0
    cpu.PC = 0x0A00
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpAddr = 0x55'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x81 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x02 # Memory updated with shifted value
      not cpu.Z                      # Zero flag clear (02 != 0)
      not cpu.N                      # Negative flag clear (bit 7 of 02 is 0)
      cpu.C == true                  # Carry flag set (original bit 7 of 81 was 1)
      cpu.PC == 0x0A02               # PC advanced by 2
      cpu.cycles == 5                # Cycles correct
  
  test "ASL ZeroPage - Sets Zero Flag":
    # Setup: ASL $66 (06 66)
    # Value at $0066 is $80 (10000000)
    # Expected: Memory[$0066] = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x0B00
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared
    cpu.cycles = 0
    let zpAddr = 0x66'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x80 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x00 # Memory updated with shifted value
      cpu.Z == true                  # Zero flag set (result is 00)
      not cpu.N                      # Negative flag clear (bit 7 of 00 is 0)
      cpu.C == true                  # Carry flag set (original bit 7 of 80 was 1)
      cpu.PC == 0x0B02               # PC advanced by 2
      cpu.cycles == 5                # Cycles correct
  
  test "ASL ZeroPage - Sets Negative Flag":
    # Setup: ASL $77 (06 77)
    # Value at $0077 is $40 (01000000)
    # Expected: Memory[$0077] = $80 (10000000), C=0, Z=0, N=1
    cpu.PC = 0x0C00
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared
    cpu.cycles = 0
    let zpAddr = 0x77'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x40 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x80 # Memory updated with shifted value
      not cpu.Z                      # Zero flag clear (80 != 0)
      cpu.N == true                  # Negative flag set (bit 7 of 80 is 1)
      not cpu.C                      # Carry flag clear (original bit 7 of 40 was 0)
      cpu.PC == 0x0C02               # PC advanced by 2
      cpu.cycles == 5                # Cycles correct
  
  
