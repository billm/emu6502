import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../fixtures/emulator_setup

suite "AND Opcode Tests":

  test "0x21 - AND (Indirect,X) - Basic Operation":
    var cpu = initCpu()
    var mem = initMemory()
    cpu.mem = mem

    # Setup initial state
    cpu.a = 0b11001100 # Accumulator value
    cpu.x = 0x04       # X register offset
    cpu.pc = 0x0200    # Program Counter start

    # Instruction: AND ($80,X)
    mem.write(cpu.pc, 0x21)     # Opcode
    mem.write(cpu.pc + 1, 0x80) # Zero page address operand

    # Indirect address calculation:
    # Base address = $80 + X = $80 + $04 = $84
    # Effective address pointer location = $0084 and $0085
    mem.write(0x0084, 0x34) # Low byte of effective address
    mem.write(0x0085, 0x12) # High byte of effective address -> Effective Address = $1234

    # Value at effective address
    mem.write(0x1234, 0b10101010) # Value to AND with

    # Expected result: 0b11001100 & 0b10101010 = 0b10001000
    let expectedResult: byte = 0b10001000
    let initialCycles = cpu.cycles

    # Execute instruction
    cpu.step()

    # Assertions
    check cpu.a == expectedResult
    check cpu.pc == 0x0202 # PC should increment by 2
    check cpu.cycles == initialCycles + 6 # Cycle count
    check cpu.flags.z == false # Zero flag should be clear
    check cpu.flags.n == true  # Negative flag should be set (bit 7 is 1)

  test "0x21 - AND (Indirect,X) - Zero Flag Set":
    var cpu = initCpu()
    var mem = initMemory()
    cpu.mem = mem

    cpu.a = 0b00110011
    cpu.x = 0x02
    cpu.pc = 0x0300

    mem.write(cpu.pc, 0x21)
    mem.write(cpu.pc + 1, 0x10) # Base address $10 + X($02) = $12

    mem.write(0x0012, 0xCD) # Effective address pointer low byte
    mem.write(0x0013, 0xAB) # Effective address pointer high byte -> $ABCD

    mem.write(0xABCD, 0b11001100) # Value to AND with

    # Expected result: 0b00110011 & 0b11001100 = 0b00000000
    let expectedResult: byte = 0b00000000
    let initialCycles = cpu.cycles

    cpu.step()

    check cpu.a == expectedResult
    check cpu.pc == 0x0302
    check cpu.cycles == initialCycles + 6
    check cpu.flags.z == true  # Zero flag should be set
    check cpu.flags.n == false # Negative flag should be clear

  test "0x21 - AND (Indirect,X) - Negative Flag Clear":
    var cpu = initCpu()
    var mem = initMemory()
    cpu.mem = mem

    cpu.a = 0b01110000
    cpu.x = 0x0F
    cpu.pc = 0x0400

    mem.write(cpu.pc, 0x21)
    mem.write(cpu.pc + 1, 0x20) # Base address $20 + X($0F) = $2F

    mem.write(0x002F, 0xEF) # Effective address pointer low byte
    mem.write(0x0030, 0xBE) # Effective address pointer high byte -> $BEEF

    mem.write(0xBEEF, 0b00001111) # Value to AND with

    # Expected result: 0b01110000 & 0b00001111 = 0b00000000
    let expectedResult: byte = 0b00000000
    let initialCycles = cpu.cycles

    cpu.step()

    check cpu.a == expectedResult
    check cpu.pc == 0x0402
    check cpu.cycles == initialCycles + 6
    check cpu.flags.z == true  # Zero flag should be set
    check cpu.flags.n == false # Negative flag should be clear (bit 7 is 0)

  test "0x21 - AND (Indirect,X) - Zero Page Wrap":
    var cpu = initCpu()
    var mem = initMemory()
    cpu.mem = mem

    cpu.a = 0b11111111
    cpu.x = 0x05
    cpu.pc = 0x0500

    mem.write(cpu.pc, 0x21)
    mem.write(cpu.pc + 1, 0xFE) # Base address $FE + X($05) = $103, wraps to $03

    # Effective address pointer location = $0003 and $0004 (due to wrap)
    mem.write(0x0003, 0x78) # Low byte of effective address
    mem.write(0x0004, 0x56) # High byte of effective address -> Effective Address = $5678

    mem.write(0x5678, 0b10101010) # Value to AND with

    # Expected result: 0b11111111 & 0b10101010 = 0b10101010
    let expectedResult: byte = 0b10101010
    let initialCycles = cpu.cycles

    cpu.step()

    check cpu.a == expectedResult
    check cpu.pc == 0x0502
    check cpu.cycles == initialCycles + 6
    check cpu.flags.z == false
    check cpu.flags.n == true