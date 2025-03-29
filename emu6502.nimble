# Package

version       = "0.1.0"
author        = "Bill Marquette"
description   = "Basic 6502 cpu emulator"
license       = "MIT"

srcDir        = "src"
bin           = @["emu6502"]

# Dependencies

requires "nim >= 0.17.2"
requires "docopt >= 0.6.7" # Argument parsing

# Tasks
task test, "Run test suite":
  exec "nim c -r tests/unit/test_cpu.nim"
  exec "nim c -r tests/unit/test_memory.nim"
  exec "nim c -r tests/unit/test_opcodes.nim"
  exec "nim c -r tests/unit/test_opcode_ANC.nim"
  exec "nim c -r tests/unit/test_opcode_ASL.nim"
  exec "nim c -r tests/unit/test_opcode_BRK.nim"
  exec "nim c -r tests/unit/test_opcode_KIL.nim"
  exec "nim c -r tests/unit/test_opcode_LDA.nim"
  exec "nim c -r tests/unit/test_opcode_NOP.nim"
  exec "nim c -r tests/unit/test_opcode_ORA.nim"
  exec "nim c -r tests/unit/test_opcode_PHP.nim"
  exec "nim c -r tests/unit/test_opcode_SLO.nim"
  exec "nim c -r tests/integration/test_programs.nim"

