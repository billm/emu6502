import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/flags
import ../../src/opcodes
import ../fixtures/emulator_setup

suite "BIT Opcode Tests":

  test "0x24 BIT ZeroPage: Z=1, N=0, V=0":
    var cpu = newCPU()
    var mem = newMemory()
    setupEmulator(cpu, mem)

    # Initial state
    cpu.A = 0b11110000 # Accumulator
    cpu.PC = 0x0200
    mem.write(0x0200, 0x24) # BIT ZeroPage opcode
    mem.write(0x0201, 0x40) # Zero Page address operand
    mem.write(0x0040, 0b00001111) # Value at Zero Page address (A & M == 0)

    # Expected state after execution
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let initialA = cpu.A

    # Execute
    cpu.step(mem)

    # Assertions
    check cpu.PC == initialPC + 2 # PC increments by 2 bytes
    check cpu.cycles == initialCycles + 3 # Takes 3 cycles
    check cpu.A == initialA # Accumulator is unchanged
    check cpu.getFlag(Flag.Z) == true # Zero flag is set (A & M == 0)
    check cpu.getFlag(Flag.N) == false # Negative flag is clear (M bit 7 is 0)
    check cpu.getFlag(Flag.V) == false # Overflow flag is clear (M bit 6 is 0)

  test "0x24 BIT ZeroPage: Z=0, N=1, V=1":
    var cpu = newCPU()
    var mem = newMemory()
    setupEmulator(cpu, mem)

    # Initial state
    cpu.A = 0b11000001 # Accumulator
    cpu.PC = 0x0200
    mem.write(0x0200, 0x24) # BIT ZeroPage opcode
    mem.write(0x0201, 0x80) # Zero Page address operand
    mem.write(0x0080, 0b11000001) # Value at Zero Page address (A & M != 0, N=1, V=1)

    # Expected state after execution
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let initialA = cpu.A

    # Execute
    cpu.step(mem)

    # Assertions
    check cpu.PC == initialPC + 2 # PC increments by 2 bytes
    check cpu.cycles == initialCycles + 3 # Takes 3 cycles
    check cpu.A == initialA # Accumulator is unchanged
    check cpu.getFlag(Flag.Z) == false # Zero flag is clear (A & M != 0)
    check cpu.getFlag(Flag.N) == true # Negative flag is set (M bit 7 is 1)
    check cpu.getFlag(Flag.V) == true # Overflow flag is set (M bit 6 is 1)

  test "0x24 BIT ZeroPage: Z=0, N=0, V=1":
    var cpu = newCPU()
    var mem = newMemory()
    setupEmulator(cpu, mem)

    # Initial state
    cpu.A = 0b01000010 # Accumulator
    cpu.PC = 0x0200
    mem.write(0x0200, 0x24) # BIT ZeroPage opcode
    mem.write(0x0201, 0x10) # Zero Page address operand
    mem.write(0x0010, 0b01000010) # Value at Zero Page address (A & M != 0, N=0, V=1)

    # Expected state after execution
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let initialA = cpu.A

    # Execute
    cpu.step(mem)

    # Assertions
    check cpu.PC == initialPC + 2 # PC increments by 2 bytes
    check cpu.cycles == initialCycles + 3 # Takes 3 cycles
    check cpu.A == initialA # Accumulator is unchanged
    check cpu.getFlag(Flag.Z) == false # Zero flag is clear (A & M != 0)
    check cpu.getFlag(Flag.N) == false # Negative flag is clear (M bit 7 is 0)
    check cpu.getFlag(Flag.V) == true # Overflow flag is set (M bit 6 is 1)

  test "0x24 BIT ZeroPage: Z=0, N=1, V=0":
    var cpu = newCPU()
    var mem = newMemory()
    setupEmulator(cpu, mem)

    # Initial state
    cpu.A = 0b10000100 # Accumulator
    cpu.PC = 0x0200
    mem.write(0x0200, 0x24) # BIT ZeroPage opcode
    mem.write(0x0201, 0x25) # Zero Page address operand
    mem.write(0x0025, 0b10000100) # Value at Zero Page address (A & M != 0, N=1, V=0)

    # Expected state after execution
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let initialA = cpu.A

    # Execute
    cpu.step(mem)

    # Assertions
    check cpu.PC == initialPC + 2 # PC increments by 2 bytes
    check cpu.cycles == initialCycles + 3 # Takes 3 cycles
    check cpu.A == initialA # Accumulator is unchanged
    check cpu.getFlag(Flag.Z) == false # Zero flag is clear (A & M != 0)
    check cpu.getFlag(Flag.N) == true # Negative flag is set (M bit 7 is 1)
    check cpu.getFlag(Flag.V) == false # Overflow flag is clear (M bit 6 is 0)