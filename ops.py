OP_READ_BIT = 0x00
OP_READ_BYTE = 0x01
OP_READ_WORD = 0x02
OP_READ_THREE = 0x03

OP_ADDR_BIT = 0x10
OP_ADDR_BYTE = 0x11
OP_ADDR_WORD = 0x12
OP_ADDR_THREE = 0x13

OP_ADD = 0x30
OP_SUB = 0x31
OP_MUL = 0x32
OP_DIV = 0x33
OP_MOD = 0x34
OP_BITWISE_AND = 0x35
OP_BITWISE_OR = 0x36
OP_BITWISE_NOT = 0x37

OP_EQ = 0x40
OP_NE = 0x41
OP_GE = 0x42
OP_LE = 0x43
OP_GT = 0x44
OP_LT = 0x45

OP_AND = 0x50
OP_OR = 0x51
OP_NOT = 0x52

OP_PUSH_BYTE = 0x60
OP_PUSH_WORD = 0x61
OP_PUSH_THREE = 0x62

OP_JZ = 0x70
OP_CASE = 0x71  # Jumps to script address in argument if pop and top of stack are not equal
OP_JP = 0x72
OP_END = 0x73
OP_POP_74 = 0x74  # unused
OP_LINK_CHARACTER = 0x75

OP_MASK = 0x80
OP_RANDOM = 0x81
OP_RANDOM_BIT = 0x82
OP_COUNT = 0x83  # If pop is Type 01, Type 01 with count of number of bits set in pop
OP_FIRST = 0x83  # If pop is Type 02, Type 02 filled with value of first non-null value in pop
OP_GREATEST = 0x84
OP_LEAST = 0x85
OP_MP_COST = 0x86
OP_SHIFT = 0x87  # Type 02 with only bit in pop turned on (1 << [pop])

OP_WRITE = 0x90  # If first pop < 4000h; Stores second pop at first pop
# If first pop >= 4000h; Stores second pop at first pop constrained by mask at third pop
OP_WRITE_MASK = 0x90
OP_POP = 0x91
OP_ATTACK = 0x92
OP_SAY = 0x93  # followed by FF-terminated string
OP_COPY_UNIT = 0x94
OP_LOADSAVE = 0x95  # reads/writes from save memory block
OP_ELEMENTAL_DEFENCE = 0x96

OP_DEBUG = 0xA0  # followed by FF-terminated string
OP_POP2_A1 = 0xA1  # unused
