import os
import strformat
import strutils

import cpu as processor


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

  # Copy our code into memory
  for index, val in code:
    mem[index + 0x300] = val
    
    
  # Initialize the CPU
  cpu.initialize(mem, 0x300)

  # Let's run some code!
  cpu.execute()
