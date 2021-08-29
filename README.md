# emu6502

emu6502 is a basic emulator that implements the MOS6502 opcodes. This is not intended to be a feature complete emulator or necessarily to provide emulation for hardware outside of the CPU. This project came to be as a means to learn nim and gain some familiarity with how the 6502 processor operated. As such, reliance on this project or any belief that the code contained is representative of "the nim way" is discouraged.

## Build

```bash
nimble build
```

## Usage

```bash
Usage:
  emu6502 [options] <program_file>
Options:
  --debug-cpu          Enable CPU register debugging
  --debug-opcodes      Enable opcode debugging
  -h --help            show this help message and exit
```

Example:  
```bash
./emu6502 tests/poc
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Thanks
Many thanks to Dennis Felsing for NimES (<https://github.com/def-/nimes>) from which I took much inspiration and code.
A huge shout out goes to Art Green who provided me with many 6502 resources and his own wealth of knowledge on the processor.

## License

[MIT](https://choosealicense.com/licenses/mit/)