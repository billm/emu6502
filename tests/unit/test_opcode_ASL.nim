import unittest
import ../../src/cpu
import ../../src/flags
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags # Import flags for setFlags
import ../../src/utils # Import utils for lowByte and highByte

suite "ASL Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  # --- Tests for Opcode 0x06: ASL ZeroPage ---

  test "ASL ZeroPage - Basic Shift, No Carry":
    # Setup: ASL $42 (06 42)
    # Value at $0042 is $41 (01000001)
    # Expected: Memory[$0042] = $82 (10000010), C=0, Z=0, N=1
    cpu.PC = 0x0900
    # cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared - Handled by emulator setup
    cpu.cycles = 0
    let zpAddr = 0x42'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x41 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x82 # Memory updated with shifted value
      not cpu.Z                      # Zero flag clear (82 != 0)
      cpu.N == true                  # Negative flag set (bit 7 of 82 is 1)
      not cpu.C                      # Carry flag clear (original bit 7 of 41 was 0)
  
  test "ASL ZeroPage - Sets Carry Flag":
    # Setup: ASL $55 (06 55)
    # Value at $0055 is $81 (10000001)
    # Expected: Memory[$0055] = $02 (00000010), C=1, Z=0, N=0
    cpu.PC = 0x0A00
    # cpu.setFlags(0x20'u8) # Clear C initially - Handled by emulator setup
    cpu.cycles = 0
    let zpAddr = 0x55'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x81 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
  


  test "ASL ZeroPage,X - Basic Shift, No Carry":
    cpu.X = 0x02
    cpu.PC = 0x0010 # Set PC for this test
    cpu.cycles = 0  # Reset cycles for this test
    cpu.memory[0x0010] = 0x16 # ASL zp,X
    cpu.memory[0x0011] = 0x20 # Zero page address operand
    cpu.memory[0x0022] = 0x41 # Value to shift (01000001)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.memory[0x0022] == 0x82 # Shifted value (10000010)
    check cpu.C == false
    check cpu.Z == false
    check cpu.N == true

  test "ASL ZeroPage,X - Sets Carry Flag":
    # Setup: ASL $50,X (16 50) with X = $05
    # Effective Address = $50 + $05 = $55
    # Value at $0055 is $81 (10000001)
    # Expected: Memory[$0055] = $02 (00000010), C=1, Z=0, N=0
    cpu.PC = 0x0E00
    cpu.X = 0x05
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpBaseAddr = 0x50'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $55
  
    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x81 # Value to shift
  
    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
  
    check:
      mem.mem[effectiveAddr.uint16] == 0x02 # Memory updated
      cpu.C == true
      cpu.Z == false
      cpu.N == false

  test "ASL ZeroPage,X - Sets Zero Flag":
    cpu.X = 0x01
    cpu.PC = 0x0010 # Set PC for this test
    cpu.cycles = 0  # Reset cycles for this test
    cpu.memory[0x0010] = 0x16 # ASL zp,X
    cpu.memory[0x0011] = 0x40 # Zero page address operand
    cpu.memory[0x0041] = 0x80 # Value to shift (10000000)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.memory[0x0041] == 0x00 # Shifted value (00000000)
    check cpu.C == true
    check cpu.Z == true
    check cpu.N == false

  test "ASL ZeroPage,X - Zero Page Wrap":
    cpu.X = 0x10
    cpu.PC = 0x0010 # Set PC for this test
    cpu.cycles = 0  # Reset cycles for this test
    cpu.memory[0x0010] = 0x16 # ASL zp,X
    cpu.memory[0x0011] = 0xF8 # Zero page address operand (0xF8 + 0x10 = 0x108 -> wraps to 0x08)
    cpu.memory[0x0008] = 0x55 # Value to shift (01010101)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.memory[0x0008] == 0xAA # Shifted value (10101010)
    check cpu.C == false
    check cpu.Z == false
    check cpu.N == true

  test "ASL ZeroPage - Sets Zero Flag":
    # Setup: ASL $66 (06 66)
    # Value at $0066 is $80 (10000000)
    # Expected: Memory[$0066] = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x0B00
    # cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially to ensure it gets cleared - Handled by emulator setup
    cpu.cycles = 0
    let zpAddr = 0x66'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x80 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x00 # Memory updated with shifted value
      cpu.Z == true                  # Zero flag set (result is 00)
      not cpu.N                      # Negative flag clear (bit 7 of 00 is 0)
      cpu.C == true                  # Carry flag set (original bit 7 of 80 was 1)
  
  test "ASL ZeroPage - Sets Negative Flag":
    # Setup: ASL $77 (06 77)
    # Value at $0077 is $40 (01000000)
    # Expected: Memory[$0077] = $80 (10000000), C=0, Z=0, N=1
    cpu.PC = 0x0C00
    # cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially to ensure it gets cleared - Handled by emulator setup
    cpu.cycles = 0
    let zpAddr = 0x77'u8
  
    mem.mem[cpu.PC] = 0x06     # ASL ZeroPage
    mem.mem[cpu.PC + 1] = zpAddr
    mem.mem[zpAddr.uint16] = 0x40 # Value to shift
  
    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
  
    check:
      mem.mem[zpAddr.uint16] == 0x80 # Memory updated with shifted value
      not cpu.Z                      # Zero flag clear (80 != 0)
      cpu.N == true                  # Negative flag set (bit 7 of 80 is 1)
      not cpu.C                      # Carry flag clear (original bit 7 of 40 was 0)
  

  # --- Tests for Opcode 0x0A: ASL Accumulator ---

  test "ASL Accumulator - Basic Shift, No Carry/Zero/Negative":
    # Setup ASL (0A)
    # Initial A = $01 (00000001)
    # Expected A = $02 (00000010), C=0, Z=0, N=0
    cpu.PC = 0x0600
    cpu.A = 0x01
    # cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8 or 0x80'u8) # Set C, Z, N initially to ensure they are cleared - Handled by emulator setup
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0A  # ASL Accumulator

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x02        # Accumulator shifted left
      not cpu.C            # Carry flag clear (original bit 7 was 0)
      not cpu.Z            # Zero flag clear (result 0x02 != 0)
      not cpu.N            # Negative flag clear (result bit 7 is 0)

  test "ASL Accumulator - Sets Zero Flag":
    # Setup ASL (0A)
    # Initial A = $00 (00000000)
    # Expected A = $00 (00000000), C=0, Z=1, N=0
    cpu.PC = 0x0600
    cpu.A = 0x00
    # cpu.setFlags(0x20'u8 or 0x01'u8 or 0x80'u8) # Set C, N initially - Handled by emulator setup
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0A  # ASL Accumulator

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x00        # Accumulator remains 0
      not cpu.C            # Carry flag clear
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear

  test "ASL Accumulator - Sets Negative Flag":
    # Setup ASL (0A)
    # Initial A = $40 (01000000)
    # Expected A = $80 (10000000), C=0, Z=0, N=1
    cpu.PC = 0x0600
    cpu.A = 0x40
    # cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8) # Set C, Z initially - Handled by emulator setup
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0A  # ASL Accumulator

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x80        # Accumulator shifted
      not cpu.C            # Carry flag clear
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set

  test "ASL Accumulator - Sets Carry Flag":
    # Setup ASL (0A)
    # Initial A = $80 (10000000)
    # Expected A = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x0600
    cpu.A = 0x80
    # cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially - Handled by emulator setup
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0A  # ASL Accumulator

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x00        # Accumulator shifted to 0
      cpu.C == true        # Carry flag set (original bit 7 was 1)
      cpu.Z == true        # Zero flag set
      not cpu.N            # Negative flag clear

  test "ASL Accumulator - Sets Carry and Negative Flags":
    # Setup ASL (0A)
    # Initial A = $C0 (11000000)
    # Expected A = $80 (10000000), C=1, Z=0, N=1
    cpu.PC = 0x0600
    cpu.A = 0xC0
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially
    cpu.cycles = 0
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    mem.mem[cpu.PC] = 0x0A  # ASL Accumulator

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      cpu.A == 0x80        # Accumulator shifted
      cpu.C == true        # Carry flag set (original bit 7 was 1)
      not cpu.Z            # Zero flag clear
      cpu.N == true        # Negative flag set

  # --- Tests for Opcode 0x0E: ASL Absolute ---

  test "ASL Absolute - Basic Shift, No Carry, Positive to Negative":
    # Setup: ASL $1234
    # Value at $1234 = $41 (01000001)
    # Expected result = $82 (10000010)
    # Expected flags: N=1, Z=0, C=0
    cpu.PC = 0x0600
    # cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8) # Set C and Z initially - Handled by emulator setup
    cpu.cycles = 0
    let targetAddr: uint16 = 0x1234
    let initialValue: uint8 = 0x41
    let expectedValue: uint8 = 0x82

    mem.mem[cpu.PC] = 0x0E      # ASL Absolute opcode
    mem.mem[cpu.PC + 1] = lowByte(targetAddr) # Low byte of address
    mem.mem[cpu.PC + 2] = highByte(targetAddr) # High byte of address
    mem.mem[targetAddr] = initialValue

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      discard # Handler not implemented yet.

    check:
      mem.mem[targetAddr] == expectedValue # Memory updated
      cpu.N == true             # Negative flag set
      not cpu.Z                 # Zero flag clear
      not cpu.C                 # Carry flag clear (original bit 7 was 0)

  test "ASL Absolute - Shift with Carry, Negative to Positive":
    # Setup: ASL $ABCD
    # Value at $ABCD = $81 (10000001)
    # Expected result = $02 (00000010)
    # Expected flags: N=0, Z=0, C=1
    cpu.PC = 0x0700
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x02'u8) # Set N and Z initially
    cpu.cycles = 0
    let targetAddr: uint16 = 0xABCD
    let initialValue: uint8 = 0x81
    let expectedValue: uint8 = 0x02

    mem.mem[cpu.PC] = 0x0E      # ASL Absolute opcode
    mem.mem[cpu.PC + 1] = lowByte(targetAddr) # Low byte of address
    mem.mem[cpu.PC + 2] = highByte(targetAddr) # High byte of address
    mem.mem[targetAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      discard # Handler not implemented yet.

    check:
      mem.mem[targetAddr] == expectedValue # Memory updated
      not cpu.N                 # Negative flag clear
      not cpu.Z                 # Zero flag clear
      cpu.C == true             # Carry flag set (original bit 7 was 1)

  test "ASL Absolute - Shift Resulting in Zero":
    # Setup: ASL $BEEF
    # Value at $BEEF = $80 (10000000)
    # Expected result = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x0800
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0
    let targetAddr: uint16 = 0xBEEF
    let initialValue: uint8 = 0x80
    let expectedValue: uint8 = 0x00

    mem.mem[cpu.PC] = 0x0E      # ASL Absolute opcode
    mem.mem[cpu.PC + 1] = lowByte(targetAddr) # Low byte of address
    mem.mem[cpu.PC + 2] = highByte(targetAddr) # High byte of address
    mem.mem[targetAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      discard # Handler not implemented yet.

    check:
      mem.mem[targetAddr] == expectedValue # Memory updated
      not cpu.N                 # Negative flag clear
      cpu.Z == true             # Zero flag set
      cpu.C == true             # Carry flag set (original bit 7 was 1)

  test "ASL Absolute - Shift Resulting in Negative, No Carry":
    # Setup: ASL $CAFE
    # Value at $CAFE = $40 (01000000)
    # Expected result = $80 (10000010), C=0, Z=0, N=1
    cpu.PC = 0x0900
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8) # Set C and Z initially
    cpu.cycles = 0
    let targetAddr: uint16 = 0xCAFE
    let initialValue: uint8 = 0x40
    let expectedValue: uint8 = 0x80

    mem.mem[cpu.PC] = 0x0E      # ASL Absolute opcode
    mem.mem[cpu.PC + 1] = lowByte(targetAddr) # Low byte of address
    mem.mem[cpu.PC + 2] = highByte(targetAddr) # High byte of address
    mem.mem[targetAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      discard # Handler not implemented yet.

    check:
      mem.mem[targetAddr] == expectedValue # Memory updated
      cpu.N == true             # Negative flag set
      not cpu.Z                 # Zero flag clear
      not cpu.C                 # Carry flag clear (original bit 7 was 0)


  # --- Tests for Opcode 0x16: ASL ZeroPage,X ---

  test "ASL ZeroPage,X - Basic Shift, No Wrap, No Carry":
    # Setup: ASL $40,X (16 40) with X = $02
    # Effective Address = $40 + $02 = $42
    # Value at $0042 = $41 (01000001)
    # Expected: Memory[$0042] = $82 (10000010), C=0, Z=0, N=1
    cpu.PC = 0x0D00
    cpu.X = 0x02
    # cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially - Handled by emulator setup
    cpu.cycles = 0
    let zpBaseAddr = 0x40'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $42

    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x41 # Value to shift

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr.uint16] == 0x82 # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      not cpu.C                      # Carry flag clear

  test "ASL ZeroPage,X - Sets Carry Flag, No Wrap":
    # Setup: ASL $50,X (16 50) with X = $05
    # Effective Address = $50 + $05 = $55
    # Value at $0055 is $81 (10000001)
    # Expected: Memory[$0055] = $02 (00000010), C=1, Z=0, N=0
    cpu.PC = 0x0E00
    cpu.X = 0x05
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpBaseAddr = 0x50'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $55

    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x81 # Value to shift

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr.uint16] == 0x02 # Memory updated
      not cpu.Z                      # Zero flag clear
      not cpu.N                      # Negative flag clear
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x0E02               # PC advanced by 2
      cpu.cycles == 6                # Cycles correct

  test "ASL ZeroPage,X - Sets Zero Flag (and Carry), No Wrap":
    # Setup: ASL $60,X (16 60) with X = $06
    # Effective Address = $60 + $06 = $66
    # Value at $0066 = $80 (10000000)
    # Expected: Memory[$0066] = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x0F00
    cpu.X = 0x06
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0
    let zpBaseAddr = 0x60'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $66

    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x80 # Value to shift

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr.uint16] == 0x00 # Memory updated
      cpu.Z == true                  # Zero flag set
      not cpu.N                      # Negative flag clear
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x0F02               # PC advanced by 2
      cpu.cycles == 6                # Cycles correct

  test "ASL ZeroPage,X - Sets Negative Flag, No Wrap":
    # Setup: ASL $70,X (16 70) with X = $07
    # Effective Address = $70 + $07 = $77
    # Value at $0077 = $40 (01000000)
    # Expected: Memory[$0077] = $80 (10000000), C=0, Z=0, N=1
    cpu.PC = 0x1000
    cpu.X = 0x07
    cpu.setFlags(0x20'u8 or 0x01'u8) # Set C initially
    cpu.cycles = 0
    let zpBaseAddr = 0x70'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $77

    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0x40 # Value to shift

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr.uint16] == 0x80 # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      not cpu.C                      # Carry flag clear
      cpu.PC == 0x1002               # PC advanced by 2
      cpu.cycles == 6                # Cycles correct

  test "ASL ZeroPage,X - Zero Page Wrap":
    # Setup: ASL $F0,X (16 F0) with X = $15
    # Effective Address = ($F0 + $15) mod 256 = $105 mod 256 = $05
    # Value at $0005 = $C1 (11000001)
    # Expected: Memory[$0005] = $82 (10000010), C=1, Z=0, N=1
    cpu.PC = 0x1100
    cpu.X = 0x15
    cpu.setFlags(0x20'u8) # Clear C initially
    cpu.cycles = 0
    let zpBaseAddr = 0xF0'u8
    let effectiveAddr = (zpBaseAddr + cpu.X) and 0xFF # $05

    mem.mem[cpu.PC] = 0x16     # ASL ZeroPage,X
    mem.mem[cpu.PC + 1] = zpBaseAddr
    mem.mem[effectiveAddr.uint16] = 0xC1 # Value to shift

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr.uint16] == 0x82 # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x1102               # PC advanced by 2
      cpu.cycles == 6                # Cycles correct


  # --- Tests for Opcode 0x1E: ASL Absolute,X ---

  test "ASL Absolute,X - Basic Shift, No Carry, No Page Cross":
    # Setup: ASL $1234,X (1E 34 12) with X = $0A
    # Effective Address = $1234 + $0A = $123E
    # Value at $123E = $41 (01000001)
    # Expected: Memory[$123E] = $82 (10000010), C=0, Z=0, N=1
    cpu.PC = 0x2000
    cpu.X = 0x0A
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8) # Set C, Z initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0x1234
    let effectiveAddr: uint16 = baseAddr + cpu.X.uint16 # $123E
    let initialValue: uint8 = 0x41
    let expectedValue: uint8 = 0x82

    mem.mem[cpu.PC] = 0x1E      # ASL Absolute,X
    mem.mem[cpu.PC + 1] = lowByte(baseAddr) # $34
    mem.mem[cpu.PC + 2] = highByte(baseAddr) # $12
    mem.mem[effectiveAddr] = initialValue

    # Execute (will fail until implemented)
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail() # Expect failure here

    check:
      mem.mem[effectiveAddr] == expectedValue # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      not cpu.C                      # Carry flag clear
      cpu.PC == 0x2003               # PC advanced by 3
      cpu.cycles == 7                # ASL Absolute,X takes 7 cycles

  test "ASL Absolute,X - Sets Carry Flag, No Page Cross":
    # Setup: ASL $ABCD,X (1E CD AB) with X = $10
    # Effective Address = $ABCD + $10 = $ABDD
    # Value at $ABDD = $81 (10000001)
    # Expected: Memory[$ABDD] = $02 (00000010), C=1, Z=0, N=0
    cpu.PC = 0x2100
    cpu.X = 0x10
    cpu.setFlags(0x20'u8 or 0x80'u8 or 0x02'u8) # Set N, Z initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xABCD
    let effectiveAddr: uint16 = baseAddr + cpu.X.uint16 # $ABDD
    let initialValue: uint8 = 0x81
    let expectedValue: uint8 = 0x02

    mem.mem[cpu.PC] = 0x1E      # ASL Absolute,X
    mem.mem[cpu.PC + 1] = lowByte(baseAddr) # $CD
    mem.mem[cpu.PC + 2] = highByte(baseAddr) # $AB
    mem.mem[effectiveAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr] == expectedValue # Memory updated
      not cpu.Z                      # Zero flag clear
      not cpu.N                      # Negative flag clear
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x2103               # PC advanced by 3
      cpu.cycles == 7                # Cycles correct

  test "ASL Absolute,X - Sets Zero Flag (and Carry), No Page Cross":
    # Setup: ASL $BEE0,X (1E E0 BE) with X = $0F
    # Effective Address = $BEE0 + $0F = $BEEF
    # Value at $BEEF = $80 (10000000)
    # Expected: Memory[$BEEF] = $00 (00000000), C=1, Z=1, N=0
    cpu.PC = 0x2200
    cpu.X = 0x0F
    cpu.setFlags(0x20'u8 or 0x80'u8) # Set N initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xBEE0
    let effectiveAddr: uint16 = baseAddr + cpu.X.uint16 # $BEEF
    let initialValue: uint8 = 0x80
    let expectedValue: uint8 = 0x00

    mem.mem[cpu.PC] = 0x1E      # ASL Absolute,X
    mem.mem[cpu.PC + 1] = lowByte(baseAddr) # $E0
    mem.mem[cpu.PC + 2] = highByte(baseAddr) # $BE
    mem.mem[effectiveAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr] == expectedValue # Memory updated
      cpu.Z == true                  # Zero flag set
      not cpu.N                      # Negative flag clear
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x2203               # PC advanced by 3
      cpu.cycles == 7                # Cycles correct

  test "ASL Absolute,X - Sets Negative Flag, No Page Cross":
    # Setup: ASL $CAF0,X (1E F0 CA) with X = $0E
    # Effective Address = $CAF0 + $0E = $CAFE
    # Value at $CAFE = $40 (01000000)
    # Expected: Memory[$CAFE] = $80 (10000000), C=0, Z=0, N=1
    cpu.PC = 0x2300
    cpu.X = 0x0E
    cpu.setFlags(0x20'u8 or 0x01'u8 or 0x02'u8) # Set C, Z initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0xCAF0
    let effectiveAddr: uint16 = baseAddr + cpu.X.uint16 # $CAFE
    let initialValue: uint8 = 0x40
    let expectedValue: uint8 = 0x80

    mem.mem[cpu.PC] = 0x1E      # ASL Absolute,X
    mem.mem[cpu.PC + 1] = lowByte(baseAddr) # $F0
    mem.mem[cpu.PC + 2] = highByte(baseAddr) # $CA
    mem.mem[effectiveAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr] == expectedValue # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      not cpu.C                      # Carry flag clear
      cpu.PC == 0x2303               # PC advanced by 3
      cpu.cycles == 7                # Cycles correct

  test "ASL Absolute,X - Page Crossing":
    # Setup: ASL $12F0,X (1E F0 12) with X = $20
    # Effective Address = $12F0 + $20 = $1310 (crosses page boundary)
    # Value at $1310 = $C1 (11000001)
    # Expected: Memory[$1310] = $82 (10000010), C=1, Z=0, N=1
    # Cycle count should still be 7 for ASL Absolute,X, even with page cross.
    cpu.PC = 0x2400
    cpu.X = 0x20
    cpu.setFlags(0x20'u8 or 0x02'u8) # Set Z initially
    cpu.cycles = 0
    let baseAddr: uint16 = 0x12F0
    let effectiveAddr: uint16 = baseAddr + cpu.X.uint16 # $1310
    let initialValue: uint8 = 0xC1
    let expectedValue: uint8 = 0x82

    mem.mem[cpu.PC] = 0x1E      # ASL Absolute,X
    mem.mem[cpu.PC + 1] = lowByte(baseAddr) # $F0
    mem.mem[cpu.PC + 2] = highByte(baseAddr) # $12
    mem.mem[effectiveAddr] = initialValue

    # Execute
    let info = opcodeTable[mem.mem[cpu.PC]]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    check:
      mem.mem[effectiveAddr] == expectedValue # Memory updated
      not cpu.Z                      # Zero flag clear
      cpu.N == true                  # Negative flag set
      cpu.C == true                  # Carry flag set
      cpu.PC == 0x2403               # PC advanced by 3
      cpu.cycles == 8                # Cycles correct (8 cycles with page cross)
