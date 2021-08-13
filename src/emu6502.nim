import os
import strformat
import strutils

type
  Memory = ref array[16384, uint8]

  CPU = ref object
    # Registers
    A: uint8
    X: uint8
    Y: uint8
    PC: uint16  # Program counter
    SP: uint8   # Stack pointer
    # CPU flags
    C: bool
    Z: bool
    I: bool
    D: bool
    B: bool
    U: bool
    V: bool
    N: bool
    memory: Memory


# Return a specific bit from provided uint8
proc bit(val: uint8, bit: range[0..7]): bool =
  ((val shr bit) and 1) != 0


# Collapse the individual CPU flags into a single 8bit field
proc flags(cpu: CPU): uint8 =
  cpu.C.uint8 or
    (cpu.Z.uint8 shl 1) or
    (cpu.I.uint8 shl 2) or
    (cpu.D.uint8 shl 3) or
    (cpu.B.uint8 shl 4) or
    (cpu.U.uint8 shl 5) or
    (cpu.V.uint8 shl 6) or
    (cpu.N.uint8 shl 7)


# Set each processor flag boolean based # on the bits set in cpu.flags
proc `flags=`(cpu: var CPU, flags: uint8) =
  cpu.C = flags.bit(0)
  cpu.Z = flags.bit(1)
  cpu.I = flags.bit(2)
  cpu.D = flags.bit(3)
  cpu.B = flags.bit(4)
  cpu.U = flags.bit(5)
  cpu.V = flags.bit(6)
  cpu.N = flags.bit(7)


# Generic debugging
proc debug(cpu: CPU) =
  echo "===REG==="
  echo &"A: 0x{toHex(cpu.A)} X: 0x{toHex(cpu.X)} Y: 0x{toHex(cpu.Y)}"
  echo &"PC: 0x{toHex(cpu.PC)} SP: 0x{toHex(cpu.SP)}"

  echo "8 bytes of RAM at memory address 0000:"
  echo &"0001: {cpu.memory[0].toHex} {cpu.memory[1].toHex} {cpu.memory[2].toHex} {cpu.memory[3].toHex}  {cpu.memory[4].toHex} {cpu.memory[5].toHex} {cpu.memory[6].toHex} {cpu.memory[7].toHex}"
  echo "========="


# Execute the opcode bytestream
proc execute(cpu: var CPU, code: seq) =

  let mem = cpu.memory
  cpu.PC=0

  echo "Registers initialized as:"
  cpu.debug

  while cpu.PC < cast[uint16](len(code)):
#    echo &"PC = {cpu.PC:04}"
    case code[cpu.PC]
    of 0x84:
      # STY
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} STY ${loc:02}"
      mem[loc] = cpu.Y
      cpu.PC += 2
    of 0x85:
      # STA
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} STA ${loc:02}"
      mem[loc] = cpu.A
      cpu.PC += 2
    of 0x86:
      # STX
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} STX ${loc:02}"
      mem[loc] = cpu.X
      cpu.PC += 2
    of 0xa4:
      # LDY
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} LDY ${loc:02}"
      cpu.Y = mem[loc]
      cpu.PC += 2
    of 0xa5:
      # LDA
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} LDA ${loc:02}"
      cpu.A = mem[loc]
      cpu.PC += 2
    of 0xa6:
      # LDX
      let loc = code[cpu.PC+1]
      echo &"{code[cpu.PC].toHex} {loc.toHex} LDX ${loc:02}"
      cpu.X = mem[loc]
      cpu.PC += 2
    else:
      echo &"Discarding unknown op code: {code[cpu.PC].toHex} @ PC: 0x{cpu.PC.toHex}"
      cpu.PC += 1
      discard

  debug(cpu)
  echo "Done"

# Set up the CPU, registers and memory as it should be
# on initialization or reset
proc initialize(cpu: var CPU, mem: Memory) =
  cpu.A = 0x41
  cpu.X = 0x42
  cpu.Y = 0x43
  cpu.C = false
  cpu.memory = mem
  echo "Initializing first 8 bytes of memory to 0xfeedface 0xdeadbeef"
  cpu.memory[0] = 0xfe
  cpu.memory[1] = 0xed
  cpu.memory[2] = 0xfa
  cpu.memory[3] = 0xce
  cpu.memory[4] = 0xde
  cpu.memory[5] = 0xad
  cpu.memory[6] = 0xbe
  cpu.memory[7] = 0xef



when isMainModule:
  var
    cpu = new CPU
    mem = new Memory
    code : seq[byte]

  echo "Emulator spinning up!"

  # Read in our code from a file
  if paramCount() > 0:
    let f = paramStr(1).open(fmRead)
    let fsize = f.getFileSize()
    code.newSeq(fsize)
    echo &"File is {fsize} bytes"
    let amountRead = f.readBytes(code, 0, fsize)
    echo &"Read in {amountRead} bytes"
    echo &"Code is now {code.len} elements long"

    var t : string
    for b in code.items:
      t.add(&"{b.toHex} ")
    echo t
  else: 
    echo "derp derp"

  # Initialize the CPU
  cpu.initialize(mem)

  # Let's run some code!
  cpu.execute(code)
