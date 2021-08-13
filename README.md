# emu6502

emu6502 is a basic emulator that implements the MOS6502 opcodes. This is not intended to be a feature complete emulator or necessarily to provide emulation for hardware outside of the CPU. This project came to be as a means to learn nim and gain some familiarity with how the 6502 processor operatoed. As such, reliance on this project or any belief that the code contained is representative of "the nim way" is discouraged.

## Build

```bash
nimble build
```

## Usage

```bash
# Argument 1 should be the 6502 raw opcode file you want to execute
./emu6502 tests/poc
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)