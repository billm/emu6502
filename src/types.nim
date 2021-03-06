type

  BackingMemory* = array[65535, uint8]

  Memory* = ref object
    mem*: BackingMemory

  CPU* = ref object
    # Registers
    A*, X*, Y*: uint8
    PC*: uint16  # Program counter
    SP*: uint8   # Stack pointer
    # CPU flags
    C*: bool # 0 bit - Carry Flag
    Z*: bool # 1 bit - Zero Flag
    I*: bool # 2 bit - Interrupt Disable
    D*: bool # 3 bit - Decimal Mode
    B*: bool # 4 bit - Break Command
    U*: bool # 5 bit - Unused
    V*: bool # 6 bit - Overflow flag
    N*: bool # 7 bit - Negative flag
    memory*: Memory
