import unittest
import ../fixtures/emulator_setup
import ../../src/cpu
import ../../src/flags
import ../../src/opcodes

suite "Opcode 0x18 - CLC (Implied)":
  var cpu: CPU

  setup:
    cpu = setupEmulator()

  test "CLC clears Carry flag when initially set":
    # Arrange
    cpu.C = true # Set Carry flag initially
    cpu.N = true # Set other flags to ensure they are not affected
    cpu.Z = true
    cpu.I = true
    cpu.D = true
    cpu.V = true
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Act
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    # Assert
    check cpu.C == false # Carry flag should be cleared
    check cpu.N == true # Other flags should remain unchanged
    check cpu.Z == true
    check cpu.I == true
    check cpu.D == true
    check cpu.V == true
    check cpu.PC == initialPC + 1 # PC should increment by 1
    check cpu.cycles == initialCycles + 2 # Cycles should increment by 2

  test "CLC clears Carry flag when initially clear":
    # Arrange
    cpu.C = false # Carry flag initially clear
    cpu.N = false # Set other flags to ensure they are not affected
    cpu.Z = false
    cpu.I = false
    cpu.D = false
    cpu.V = false
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles

    # Act
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()

    # Assert
    check cpu.C == false # Carry flag should remain cleared
    check cpu.N == false # Other flags should remain unchanged
    check cpu.Z == false
    check cpu.I == false
    check cpu.D == false
    check cpu.V == false
    check cpu.PC == initialPC + 1 # PC should increment by 1
    check cpu.cycles == initialCycles + 2 # Cycles should increment by 2