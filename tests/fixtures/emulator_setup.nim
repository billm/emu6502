import ../../src/cpu
import ../../src/memory
import ../../src/types
import ../../src/opcodes

proc createEmulator*(startAddress: uint16 = 0x300): tuple[cpu: CPU, mem: Memory] =
  ## Creates and initializes a new CPU and Memory for testing
  ## startAddress defaults to 0x300 (standard program load address)
  result.cpu = new CPU
  result.mem = new Memory
  setupOpcodeTable()
  result.cpu.initialize(result.mem, startAddress)

proc loadProgram*(mem: var Memory, filepath: string, loadAddress: uint16 = 0x300) =
  ## Loads a binary file into memory at the specified address
  ## loadAddress defaults to 0x300 (standard program load address)
  let f = open(filepath, fmRead)
  let fsize = f.getFileSize()
  var code = newSeq[byte](fsize)
  discard f.readBytes(code, 0, fsize)
  f.close()
  mem.initialize(code, loadAddress)