import types

# Set the Zero flag based on a value
proc setZ*(cpu: var CPU, val: uint8) =
  cpu.Z = val == 0

# Set the Negative flag based on a value
proc setN*(cpu: var CPU, val: uint8) =
  cpu.N = (val and 0x80) != 0


proc updateZNFlags*(cpu: var CPU, result: uint8) =
  ## Updates the Zero (Z) and Negative (N) flags based on the result value.
  cpu.Z = (result == 0)
  cpu.N = (result and 0x80) != 0 # Check if bit 7 is set

# Return a specific bit from provided uint8
proc bit(val: uint8, bit: range[0..7]): bool =
  ((val shr bit) and 1) != 0

# Collapse the individual CPU flags into a single 8bit field
proc flags*(cpu: CPU): uint8 =
  cpu.C.uint8 or
    (cpu.Z.uint8 shl 1) or
    (cpu.I.uint8 shl 2) or
    (cpu.D.uint8 shl 3) or
    (cpu.B.uint8 shl 4) or
    (cpu.U.uint8 shl 5) or
    (cpu.V.uint8 shl 6) or
    (cpu.N.uint8 shl 7)

# Set each processor flag boolean based on the bits set in cpu.flags
proc setFlags*(cpu: var CPU, flags: uint8) = # Added * to export
  cpu.C = flags.bit(0)
  cpu.Z = flags.bit(1)
  cpu.I = flags.bit(2)
  cpu.D = flags.bit(3)
  cpu.B = flags.bit(4)
  cpu.U = flags.bit(5)
  cpu.V = flags.bit(6)
  cpu.N = flags.bit(7)