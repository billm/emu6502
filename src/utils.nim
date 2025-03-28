import types
import strformat
import strutils
import memory

var debugOpcodes* = false
var debugCpu* = false

proc printOpCode*(cpu: CPU, assembly: string) =
  if debugOpcodes:
    stderr.writeLine(&"{cpu.PC.toHex}: {cpu.memory[cpu.PC].toHex}      {assembly}")

proc printOpCode*(cpu: CPU, val: uint8, assembly: string) =
  if debugOpcodes:
    stderr.writeLine(&"{cpu.PC.toHex}: {cpu.memory[cpu.PC].toHex} {val.toHex}   {assembly}")

proc printOpCode*(cpu: CPU, val: uint16, assembly: string) =
  if debugOpcodes:
    stderr.writeLine(&"{cpu.PC.toHex}: {cpu.memory[cpu.PC].toHex} {val.toHex} {assembly}")

# Generic debugging
proc debug*(cpu: CPU, override: bool = false ) =
  if debugCpu or override:
    var fh = stderr
    if override:
      fh = stdout
    fh.writeLine("===REG===")
    fh.writeLine(&"A: 0x{toHex(cpu.A)} X: 0x{toHex(cpu.X)} Y: 0x{toHex(cpu.Y)}")
    fh.writeLine(&"PC: 0x{toHex(cpu.PC)} SP: 0x{toHex(cpu.SP)}")
    fh.writeLine("=========\n") 