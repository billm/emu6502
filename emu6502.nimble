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
