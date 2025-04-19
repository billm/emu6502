import unittest
import ../fixtures/emulator_setup
import ../../src/types
import ../../src/flags

suite "BPL Opcode Tests":
  var cpu: CPU

  setup:
    cpu = initCPU()

  test "BPL should not branch if Negative flag is set":
    cpu.N = true # Set Negative flag
    cpu.memory[0x0000] = 0x10 # BPL opcode
    cpu.memory[0x0001] = 0x05 # Relative offset +5
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.PC == initialPC + 2 # PC should advance by 2 (opcode + operand)
    check cpu.cycles == initialCycles + 2 # Branch not taken costs 2 cycles
    check cpu.N == true # Flags should be unchanged

  test "BPL should branch if Negative flag is clear (same page)":
    cpu.N = false # Clear Negative flag
    cpu.memory[0x0000] = 0x10 # BPL opcode
    cpu.memory[0x0001] = 0x05 # Relative offset +5
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.PC == initialPC + 2 + 5 # PC should be initial + 2 + offset
    check cpu.cycles == initialCycles + 3 # Branch taken (same page) costs 3 cycles
    check cpu.N == false # Flags should be unchanged

  test "BPL should branch if Negative flag is clear (different page)":
    cpu.N = false # Clear Negative flag
    cpu.PC = 0x00F0 # Start near end of page
    cpu.memory[0x00F0] = 0x10 # BPL opcode
    cpu.memory[0x00F1] = 0x10 # Relative offset +16 (crosses page boundary to 0x0102)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    check cpu.PC == initialPC + 2 + 16 # PC should be initial + 2 + offset
    check cpu.PC == 0x0102 # Explicit check for page cross result
    check cpu.cycles == initialCycles + 4 # Branch taken (different page) costs 4 cycles
    check cpu.N == false # Flags should be unchanged

  test "BPL should handle negative offset branch (same page)":
    cpu.N = false # Clear Negative flag
    cpu.PC = 0x0010 # Start address
    cpu.memory[0x0010] = 0x10 # BPL opcode
    cpu.memory[0x0011] = 0xFA # Relative offset -6 (0xFA as signed byte)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    # Expected PC = 0x0010 + 2 - 6 = 0x000C
    check cpu.PC == 0x000C
    check cpu.cycles == initialCycles + 3 # Branch taken (same page) costs 3 cycles
    check cpu.N == false # Flags should be unchanged

  test "BPL should handle negative offset branch (different page)":
    cpu.N = false # Clear Negative flag
    cpu.PC = 0x0105 # Start address
    cpu.memory[0x0105] = 0x10 # BPL opcode
    cpu.memory[0x0106] = 0xFA # Relative offset -6 (0xFA as signed byte)
    let initialPC = cpu.PC
    let initialCycles = cpu.cycles
    let opcode = cpu.memory[cpu.PC]
    let info = opcodeTable[opcode]
    if info.handler != nil:
      info.handler(cpu, info)
    else:
      fail()
    # Expected PC = 0x0105 + 2 - 6 = 0x0101
    check cpu.PC == 0x0101
    check cpu.cycles == initialCycles + 4 # Branch taken (different page) costs 4 cycles
    check cpu.N == false # Flags should be unchanged