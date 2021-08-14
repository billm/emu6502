import strformat
import strutils
import types


proc `[]`*(mem: Memory, adr: uint16): uint8 =
  case adr
  of 0xfded:
    echo "ERROR"
  else:
    result = mem[adr]

proc `[]=`*(mem:Memory, adr: uint16, val: uint8) =
  case adr
  of 0xfded:
    echo &"VAL: {val.toHex}"
  else:
    mem[adr] = val

proc read16*(mem: Memory, adr: uint16): uint16 =
  mem[adr+1].uint16 shl 8 or mem[adr]