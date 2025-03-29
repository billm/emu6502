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