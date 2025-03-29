import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup

suite "Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "Unimplemented opcode raises correct exception":
    # First, clear the opcode table entry for 0xFF (likely unimplemented)
    opcodeTable[0xFF].handler = nil
    
    # Set PC to point to our test opcode
    cpu.PC = 0x300
    mem.mem[0x300] = 0xFF

    expect UnimplementedOpcodeError:
      cpu.execute()

    try:
      cpu.execute()
    except UnimplementedOpcodeError as e:
      check:
        e.opcode == 0xFF
        e.pc == 0x300
        e.msg == "Unimplemented opcode: FF @ PC: 0x0300"

  test "BRK instruction works correctly":
    # Set up IRQ vector at 0xFFFE-0xFFFF to point to a BRK handler
    # that just contains another BRK (to test proper flag setting)
    mem.mem[0x8000] = 0x00  # BRK instruction in handler
    mem.mem[0xFFFE] = 0x00  # IRQ vector low byte
    mem.mem[0xFFFF] = 0x80  # IRQ handler at 0x8000
    
    # Set up a BRK instruction
    cpu.PC = 0x300
    mem.mem[0x300] = 0x00  # BRK opcode
    
    # Execute BRK
    let oldSP = cpu.SP
    cpu.cycles = 0  # Reset cycles for checking
    
    # Execute just one instruction (the BRK)
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
    check:
      cpu.B == true         # Break flag should be set
      cpu.I == true         # Interrupt disable should be set
      cpu.PC == 0x8000     # Should jump to IRQ vector
      cpu.SP == oldSP - 3  # Should push 3 bytes (PCH, PCL, P)
      cpu.cycles >= 7      # BRK takes 7 cycles
  # As opcode handlers are implemented, add specific tests for each one.
  
  test "LDA immediate mode sets accumulator and flags - positive value":
    # Setup LDA #$42 (A9 42)
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x42  # Value to load
    cpu.cycles = 0  # Reset cycles
    
    # Execute just one instruction
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
    check:
      cpu.A == 0x42        # Accumulator has value
      not cpu.Z            # Zero flag clear (0x42 != 0)
      not cpu.N            # Negative flag clear (bit 7 = 0)
      cpu.PC == 0x302     # PC advanced past instruction + operand
      cpu.cycles == 2      # LDA immediate takes 2 cycles

  test "LDA immediate mode sets zero flag":
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x00  # Load zero
    
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
    check cpu.Z == true    # Zero flag should be set

  test "LDA immediate mode sets negative flag":
    cpu.PC = 0x300
    mem.mem[0x300] = 0xA9  # LDA immediate
    mem.mem[0x301] = 0x80  # Load negative value (bit 7 set)
    
    let info = opcodeTable[mem.mem[cpu.PC]]
    info.handler(cpu)
    
    check:
      cpu.N == true        # Negative flag should be set
      not cpu.Z            # Zero flag should be clear
  
  # More opcode tests will be added here as they are implemented,
  # following Test-Driven Development principles