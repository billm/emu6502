import unittest
import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes
import ../fixtures/emulator_setup
import ../../src/flags
import ../../src/utils

suite "BRK Opcode Unit Tests":
  var
    cpu: CPU
    mem: Memory
  
  setup:
    (cpu, mem) = createEmulator()

  test "BRK instruction works correctly":
    # Set up IRQ vector at 0xFFFE/FFFF to point to handler
    mem.mem[0xFFFE] = 0x00'u8  # Low byte of IRQ vector
    mem.mem[0xFFFF] = 0x80'u8  # IRQ handler at 0x8000
    
    # Initial CPU state
    cpu.PC = 0x0300
    cpu.SP = 0xFF'u8
    cpu.I = false  # Ensure interrupt disable starts cleared
    cpu.B = false  # Break flag should start cleared
    cpu.setFlags(0b00100000'u8)  # Only unused flag set
    cpu.cycles = 0
    
    # Set up BRK instruction
    mem.mem[0x0300] = 0x00'u8  # BRK opcode
    
    # Execute BRK 
    let info = opcodeTable[mem.mem[cpu.PC].uint8]
    info.handler(cpu)
    
    # Stack should contain PC+2 (high byte, low byte) and flags with B set
    let finalSP = cpu.SP.uint16 # Should be 0xFC
    let stackedFlags = mem.mem[0x0100 + finalSP + 1] # Status at 0x01FD
    let stackedPCLow = mem.mem[0x0100 + finalSP + 2] # PCL at 0x01FE
    let stackedPCHigh = mem.mem[0x0100 + finalSP + 3] # PCH at 0x01FF
    
    check:
      # PC + 2 was pushed correctly
      stackedPCHigh == 0x03'u8  # High byte of 0x0302
      stackedPCLow == 0x02'u8   # Low byte of 0x0302
      
      # Status was pushed with B set
      (stackedFlags and 0b00110000'u8) == 0b00110000'u8  # Both B and U set
      (stackedFlags and 0b00001111'u8) == 0b00000000'u8  # Other flags clear
      
      # CPU state after BRK
      cpu.I == true           # Interrupt disable set
      cpu.B == false         # B not set in actual status
      cpu.PC == 0x0300
      cpu.SP == 0xFF'u8 - 3'u8  # Pushed 3 bytes
      cpu.cycles == 7'u16    # BRK takes 7 cycles