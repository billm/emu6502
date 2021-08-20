import types
import strutils

export types.Memory

template print(s: varargs[string, `$`]) =
  for x in s:
    stdout.write x
  stdout.flushFile()

# Read a byte of memory from adr
proc `[]`*(memory: Memory, adr: uint16): uint8 =
  case adr
  of 0xfded:
    result = 0x85 # STA
  of 0xfdee:
    result = 0x36 # $36
  of 0xfdef:
    result = 0x60 # RTS
  else:
    result = memory.mem[adr]


# Write a byte of memory to adr
proc `[]=`*(memory: var Memory, adr: uint16, val: uint8) =
  case adr
  of 0x36:
    # Apple II print char in A
    if (val == 0x0d):
      print("\r\n")
    else:
      print(val.char)
  else:
    memory.mem[adr] = val

# Read 16 bits from memory
proc read16*(mem: Memory, adr: uint16): uint16 =
  mem[adr+1].uint16 shl 8 or mem[adr]

# Initialize memory at location
proc initialize*(mem: var Memory, code:seq, adr:uint16) =
  # Copy our code into memory
  for index, val in code:
    mem[cast[uint16](index + 0x300)] = cast[uint8](val)

proc dump*(mem: Memory) =
  for index, val in mem.mem:
    if (index mod 16 == 0):
      print("\r\n")
      print(index.toHex)
      print(": ")
    if val == 0:
      print(".")
    else:
      print(val.char)
