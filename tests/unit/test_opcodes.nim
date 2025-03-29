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
        fail("Opcode 0x02 handler is nil") 
    except UnimplementedOpcodeError:
      # This is the expected path if the handler is truly nil
      # We still need checks below to ensure *correct* implementation later
      discard # Allow test to proceed to checks for when it *is* implemented
    
    # Checks - These will fail until the opcode is correctly implemented
    check:
      cpu.halted == true                   # CPU should be halted
      cpu.PC == initialPC + 1            # PC should advance by 1
      cpu.cycles == initialCycles + 2    # Cycles should increment by 2
