import strformat
import strutils
import docopt

import cpu as processor
import memory
import opcodes
import utils

let doc = """
Usage:
  emu6502 [options] <program_file>

Options:
  --debug-cpu          Enable CPU register debugging
  --debug-opcodes      Enable opcode debugging
  -h --help            show this help message and exit
"""

let args = docopt(doc)

utils.debugCpu = args["--debug-cpu"]
utils.debugOpcodes = args["--debug-opcodes"]

let programFile = $args["<program_file>"]

var
  cpu = new CPU
  mem = new Memory
  code : seq[byte]

echo "Emulator spinning up!"

# Read in our code from a file
let f = programFile.open(fmRead)
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

# Initialize opcode table
setupOpcodeTable()
  
# Initialize memory
mem.initialize(code, 0x300)
#mem.dump()

# Initialize the CPU
cpu.initialize(mem, 0x300)

# Let's run some code!
cpu.execute()
