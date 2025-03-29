import unittest
import ../../src/memory
import ../../src/types

suite "Memory Unit Tests":
  var mem: Memory

  setup:
    mem = new Memory
    # Memory should start zeroed
    for i in 0..high(mem.mem):
      mem.mem[i] = 0

  test "Memory size is correct":
    check len(mem.mem) == 65536  # Full 64K address space (0x0000 to 0xFFFF)

  test "Memory initialization with program":
    let testProgram = @[byte 0xA9, 0x42, 0x85, 0x00]  # Example program bytes
    mem.initialize(testProgram, 0x300)

    # Verify program was loaded at correct address
    for i in 0..<testProgram.len:
      check mem.mem[0x300 + i] == testProgram[i]

    # Verify memory before program location is untouched
    for i in 0..<0x300:
      check mem.mem[i] == 0

    # Verify memory after program is untouched
    for i in (0x300 + testProgram.len)..<mem.mem.len:
      check mem.mem[i] == 0

  test "Memory bounds":
    # Write to first byte
    mem.mem[0] = 0xFF
    check mem.mem[0] == 0xFF

    # Write to last byte
    mem.mem[high(mem.mem)] = 0xFF
    check mem.mem[high(mem.mem)] == 0xFF

  test "Memory read/write patterns":
    # Write incrementing pattern
    for i in 0..255:
      mem.mem[i] = byte(i)

    # Verify pattern
    for i in 0..255:
      check mem.mem[i] == byte(i)

    # Write decrementing pattern
    for i in 0..255:
      mem.mem[i] = byte(255 - i)

    # Verify pattern
    for i in 0..255:
      check mem.mem[i] == byte(255 - i)

    # Write alternating pattern
    for i in 0..255:
      mem.mem[i] = if (i mod 2 == 0): byte(0xAA) else: byte(0x55)

    # Verify pattern
    for i in 0..255:
      check mem.mem[i] == (if (i mod 2 == 0): byte(0xAA) else: byte(0x55))