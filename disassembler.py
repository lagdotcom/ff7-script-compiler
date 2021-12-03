from io import BytesIO
from struct import unpack
from typing import BinaryIO

from ops import Op
from tools import hexBytes


class NumberOperand:
    size: int
    raw: bytes

    def __init__(self, f: BinaryIO, size: int):
        self.size = size
        self.raw = f.read(size)[::-1]

    def __repr__(self):
        return hexBytes(self.raw)


class ByteOp(NumberOperand):
    def __init__(self, f: BinaryIO):
        NumberOperand.__init__(self, f, 1)


class WordOp(NumberOperand):
    def __init__(self, f: BinaryIO):
        NumberOperand.__init__(self, f, 2)


class ThreeOp(NumberOperand):
    def __init__(self, f: BinaryIO):
        NumberOperand.__init__(self, f, 3)


class StringOperand:
    size: int
    raw: bytes

    def __init__(self, f: BinaryIO):
        size = 0
        str = bytes()
        while True:
            size += 1
            bs = f.read(1)
            b = bs[0]
            str += bs
            if b == 0 or b == 255:
                break
        self.size = size
        self.raw = str

    def __repr__(self):
        return str(self.raw[:-1], 'utf-8')


ops = {
    Op.READ_BIT: 'read.bit',
    Op.READ_BYTE: 'read.b',
    Op.READ_WORD: 'read.w',
    Op.READ_THREE: 'read.3',

    Op.ADDR_BIT: 'ref.bit',
    Op.ADDR_BYTE: 'ref.b',
    Op.ADDR_WORD: 'ref.w',
    Op.ADDR_THREE: 'ref.3',

    Op.ADD: 'add',
    Op.SUB: 'sub',
    Op.MUL: 'mul',
    Op.DIV: 'div',
    Op.MOD: 'mod',
    Op.BITWISE_AND: 'bit.and',
    Op.BITWISE_OR: 'bit.or',
    Op.BITWISE_NOT: 'bit.not',

    Op.EQ: 'eq',
    Op.NE: 'ne',
    Op.GE: 'ge',
    Op.LE: 'le',
    Op.GT: 'gt',
    Op.LT: 'lt',

    Op.AND: 'and',
    Op.OR: 'or',
    Op.NOT: 'not',

    Op.PUSH_BYTE: 'push.b',
    Op.PUSH_WORD: 'push.w',
    Op.PUSH_THREE: 'push.3',

    Op.JZ: 'jz',
    Op.CASE: 'j_case',
    Op.JP: 'jp',
    Op.END: 'end',
    Op.POP_74: 'pOp.74',
    Op.LINK_CHARACTER: 'link_char',

    Op.MASK: 'mask',
    Op.RANDOM: 'random',
    Op.RANDOM_BIT: 'random.bit',
    Op.COUNT: 'count',
    Op.GREATEST: 'set_greatest',
    Op.LEAST: 'set_least',
    Op.MP_COST: 'mp_cost',
    Op.SHIFT: 'shift',

    Op.WRITE: 'write',
    Op.POP: 'pop',
    Op.ATTACK: 'attack',
    Op.SAY: 'say',
    Op.COPY_UNIT: 'copy_unit',
    Op.LOADSAVE: 'load_save',
    Op.ELEMENTAL_DEFENCE: 'elemental_defence',

    Op.DEBUG: 'debug',
    Op.POP2_A1: 'pop2_a1'
}

args = {
    Op.READ_BIT: WordOp,
    Op.READ_BYTE: WordOp,
    Op.READ_WORD: WordOp,
    Op.READ_THREE: WordOp,

    Op.ADDR_BIT: WordOp,
    Op.ADDR_BYTE: WordOp,
    Op.ADDR_WORD: WordOp,
    Op.ADDR_THREE: WordOp,

    Op.PUSH_BYTE: ByteOp,
    Op.PUSH_WORD: WordOp,
    Op.PUSH_THREE: ThreeOp,

    Op.JZ: WordOp,
    Op.CASE: WordOp,
    Op.JP: WordOp,

    Op.SAY: StringOperand,
    Op.DEBUG: StringOperand
}


def disassemble(f: BinaryIO):
    i = 0
    while True:
        old = f.tell()
        code = f.read(1)
        if not len(code):
            break
        op = Op(code[0])
        if op not in ops:
            print("Unknown opcode: %s" % op)
            break
        mne = ops[op]
        if op in args:
            arg = args[op](f)
            argHex = hexBytes(arg.raw)[1:]
            if len(argHex) > 6:
                argHex = argHex[:3] + '...'
        else:
            arg = ''
            argHex = ''
        print("%03x %02x\t%-6s %s %s" % (i, code[0], argHex, mne, arg))
        i += f.tell() - old


if __name__ == "__main__":
    raw = b'\x01\x00\x00\x60\x00\x71\x00\x0b\x72\x00\x10\x60\x01\x71\x00\x45\x12\x20\x70\x02\x20\xa0\x82\x90\x60\x20\x61\x02\x49\x92\x81\x01\x00\x20\x34\x52\x70\x00\x35\x12\x20\x70\x02\x20\xa0\x82\x90'
    f = BytesIO(raw)
    disassemble(f)
