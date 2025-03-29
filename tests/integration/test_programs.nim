import unittest
import ../fixtures/emulator_setup
import ../../src/cpu
import ../../src/memory
import ../../src/types

suite "Program Integration Tests":
  var
    cpu: CPU
    mem: Memory

  setup:
    (cpu, mem) = createEmulator()

  test "helloworld program loads and executes correctly":
    mem.loadProgram("tests/helloworld")

    # Verify the program is loaded correctly in memory
    check:
      mem.mem[0x300] == 0xA2  # LDX #0
      mem.mem[0x301] == 0x00  # 
      mem.mem[0x302] == 0xBD  # LDA $030E,X
      mem.mem[0x30E] == 0x68  # 'h' - Start of "hello world" string
      mem.mem[0x319] == 0x0D  # Carriage return
      mem.mem[0x31A] == 0x00  # Null terminator

    # Set up IRQ vector for BRK instruction to halt execution
    mem.mem[0x8000] = 0x00  # BRK instruction in handler
    mem.mem[0xFFFE] = 0x00  # IRQ vector low byte
    mem.mem[0xFFFF] = 0x80  # IRQ vector high byte (points to 0x8000)
    
    # Save initial state for verification
    let oldSP = cpu.SP
    cpu.cycles = 0
    
    # Execute program - should print "hello world" via $FDED and exit via BRK
    cpu.execute()
    
    check:
      cpu.B == true  # Break flag should be set
      cpu.SP == oldSP - 6  # 3 bytes for each BRK instruction (one at end of program, one in IRQ handler)
      cpu.X == 0x0C  # X should be 12 (looped through all characters + null)

  test "bf6502 program handles unimplemented opcode":
    mem.loadProgram("tests/bf6502")

    # Execute and expect the unimplemented opcode error
    expect UnimplementedOpcodeError:
      cpu.execute()