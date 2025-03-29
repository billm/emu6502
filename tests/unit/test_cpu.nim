import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup

suite "CPU Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "CPU initialization sets correct default values":
    check:
      cpu.A == 0x0
      cpu.X == 0x0
      cpu.Y == 0x0
      cpu.SP == 0xff
      cpu.PC == 0x300  # Our standard program load address
      cpu.cycles == 0
      not cpu.C  # Carry flag
      not cpu.Z  # Zero flag
      not cpu.I  # Interrupt disable
      not cpu.D  # Decimal mode
      not cpu.B  # Break command
      not cpu.V  # Overflow flag
      not cpu.N  # Negative flag

  test "CPU flags can be set and cleared":
    # Set all flags
    cpu.C = true
    cpu.Z = true
    cpu.I = true
    cpu.D = true
    cpu.B = true
    cpu.V = true
    cpu.N = true

    check:
      cpu.C  # Carry flag
      cpu.Z  # Zero flag
      cpu.I  # Interrupt disable
      cpu.D  # Decimal mode
      cpu.B  # Break command
      cpu.V  # Overflow flag
      cpu.N  # Negative flag

    # Clear all flags
    cpu.C = false
    cpu.Z = false
    cpu.I = false
    cpu.D = false
    cpu.B = false
    cpu.V = false
    cpu.N = false

    check:
      not cpu.C  # Carry flag
      not cpu.Z  # Zero flag
      not cpu.I  # Interrupt disable
      not cpu.D  # Decimal mode
      not cpu.B  # Break command
      not cpu.V  # Overflow flag
      not cpu.N  # Negative flag

  test "Memory is correctly initialized with test pattern":
    # Check the test pattern written by CPU.initialize
    check:
      mem.mem[0] == 0xfe  # First byte of 0xfeedface
      mem.mem[1] == 0xed
      mem.mem[2] == 0xfa
      mem.mem[3] == 0xce
      mem.mem[4] == 0xde  # First byte of 0xdeadbeef
      mem.mem[5] == 0xad
      mem.mem[6] == 0xbe
      mem.mem[7] == 0xef