import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "NOP Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

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

  # --- Tests for Opcode 0x14: NOP ZeroPage,X (Unofficial) ---

  test "NOP ZeroPage,X (0x14) - No Operation":
    # Setup: NOP $50,X (14 50) with X = $10
    # Action: Reads from $0060 (50 + 10) but does nothing with the value.
    # Expected: PC+=2, Cycles+=4. A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0x10 # Key for ZeroPage,X
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100) # Set some flags initially
    cpu.cycles = 10

    mem.mem[0x0800] = 0x14  # NOP ZeroPage,X opcode
    mem.mem[0x0801] = 0x50  # Zero page address operand
    mem.mem[0x0060] = 0xDD  # Value at the effective zero page address (0x50 + 0x10)

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
      fail()

    check:
      # State unchanged
      cpu.A == initialA
      cpu.X == initialX # X should not change
      cpu.Y == initialY
      cpu.SP == initialSP
      cpu.flags() == initialFlags

      # PC and Cycles updated
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 4

  test "NOP ZeroPage,X (0x14) - Zero Page Wrap":
    # Setup: NOP $F5,X (14 F5) with X = $10
    # Action: Reads from $0005 (F5 + 10 wrap) but does nothing.
    # Expected: PC+=2, Cycles+=4. A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0x10 # Key for ZeroPage,X
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100)
    cpu.cycles = 10

    mem.mem[0x0800] = 0x14  # NOP ZeroPage,X opcode
    mem.mem[0x0801] = 0xF5  # Zero page address operand
    mem.mem[0x0005] = 0xEE  # Value at the wrapped effective zero page address (0xF5 + 0x10 = 0x105 -> 0x05)

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
      fail()

    check:
      # State unchanged
      cpu.A == initialA
      cpu.X == initialX
      cpu.Y == initialY
      cpu.SP == initialSP
      cpu.flags() == initialFlags

      # PC and Cycles updated
      cpu.PC == initialPC + 2
      cpu.cycles == initialCycles + 4


  # --- Tests for Opcode 0x1A: NOP Implied (Unofficial) ---

  test "NOP Implied (0x1A) - No Operation":
    # Setup: NOP (1A)
    # Action: Does nothing.
    # Expected: PC+=1, Cycles+=2. A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0xBB
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100) # Set some flags initially
    cpu.cycles = 10

    mem.mem[0x0800] = 0x1A  # NOP Implied opcode

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
      cpu.PC == initialPC + 1
      cpu.cycles == initialCycles + 2


  # --- Tests for Opcode 0x1C: NOP Absolute,X (Unofficial) ---

  test "NOP Absolute,X (0x1C) - No Page Cross":
    # Setup: NOP $1234,X (1C 34 12) with X = $10
    # Action: Reads from $1244 (1234 + 10) but does nothing.
    # Expected: PC+=3, Cycles+=4. A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0x10 # Key for Absolute,X
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100) # Set some flags initially
    cpu.cycles = 10

    mem.mem[0x0800] = 0x1C  # NOP Absolute,X opcode
    mem.mem[0x0801] = 0x34  # Low byte of address
    mem.mem[0x0802] = 0x12  # High byte of address
    mem.mem[0x1244] = 0xDD  # Value at the effective address (0x1234 + 0x10)

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
      fail()

    check:
      # State unchanged
      cpu.A == initialA
      cpu.X == initialX # X should not change
      cpu.Y == initialY
      cpu.SP == initialSP
      cpu.flags() == initialFlags

      # PC and Cycles updated
      cpu.PC == initialPC + 3
      cpu.cycles == initialCycles + 4 # 4 cycles, no page cross

  test "NOP Absolute,X (0x1C) - Page Cross":
    # Setup: NOP $12F0,X (1C F0 12) with X = $20
    # Action: Reads from $1310 (12F0 + 20, crosses page) but does nothing.
    # Expected: PC+=3, Cycles+=5 (4 + 1 for page cross). A, X, Y, SP, Flags unchanged.
    cpu.PC = 0x0800
    cpu.A = 0xAA
    cpu.X = 0x20 # Key for Absolute,X
    cpu.Y = 0xCC
    cpu.SP = 0xFD
    cpu.setFlags(0b11001100) # Set some flags initially
    cpu.cycles = 10

    mem.mem[0x0800] = 0x1C  # NOP Absolute,X opcode
    mem.mem[0x0801] = 0xF0  # Low byte of address
    mem.mem[0x0802] = 0x12  # High byte of address
    mem.mem[0x1310] = 0xEE  # Value at the effective address (0x12F0 + 0x20 = 0x1310)

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
      fail()

    check:
      # State unchanged
      cpu.A == initialA
      cpu.X == initialX # X should not change
      cpu.Y == initialY
      cpu.SP == initialSP
      cpu.flags() == initialFlags

      # PC and Cycles updated
      cpu.PC == initialPC + 3
      cpu.cycles == initialCycles + 5 # 4 cycles + 1 for page cross

