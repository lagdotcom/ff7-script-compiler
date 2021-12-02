from io import BytesIO
from struct import unpack
from typing import BinaryIO

from ops import OP_ADD, OP_ADDR_BIT, OP_ADDR_BYTE, OP_ADDR_THREE, OP_ADDR_WORD, OP_AND, OP_ATTACK, OP_BITWISE_AND, OP_BITWISE_NOT, OP_BITWISE_OR, OP_COPY_UNIT, OP_COUNT, OP_DEBUG, OP_DIV, OP_ELEMENTAL_DEFENCE, OP_END, OP_EQ, OP_GE, OP_GREATEST, OP_GT, OP_CASE, OP_JP, OP_JZ, OP_LE, OP_LEAST, OP_LINK_CHARACTER, OP_LOADSAVE, OP_LT, OP_MASK, OP_MOD, OP_MP_COST, OP_MUL, OP_NE, OP_NOT, OP_OR, OP_POP, OP_POP2_A1, OP_POP_74, OP_PUSH_BYTE, OP_PUSH_THREE, OP_PUSH_WORD, OP_RANDOM, OP_RANDOM_BIT, OP_READ_BIT, OP_READ_BYTE, OP_READ_THREE, OP_READ_WORD, OP_SAY, OP_SHIFT, OP_SUB, OP_WRITE


def hexBytes(by: bytes) -> str:
    s = '$'
    for b in by:
        s += '%02x' % b
    return s


class Op:
    def __init__(self, f: BinaryIO, size: int):
        self.size = size
        self.raw = f.read(size)

    def __repr__(self):
        return hexBytes(self.raw)


class ByteOp(Op):
    def __init__(self, f: BinaryIO):
        Op.__init__(self, f, 1)


class WordOp(Op):
    def __init__(self, f: BinaryIO):
        Op.__init__(self, f, 2)


class ThreeOp(Op):
    def __init__(self, f: BinaryIO):
        Op.__init__(self, f, 3)


class StringOp:
    def __init__(self, f: BinaryIO):
        size = 0
        str = bytes()
        while True:
            size += 1
            bs = f.read(1)
            b = bs[0]
            if b == 0 or b == 255:
                break
            str += bs
        self.size = size
        self.raw = str


ops = {
    OP_READ_BIT: 'read.bit',
    OP_READ_BYTE: 'read.b',
    OP_READ_WORD: 'read.w',
    OP_READ_THREE: 'read.3',

    OP_ADDR_BIT: 'ref.bit',
    OP_ADDR_BYTE: 'ref.b',
    OP_ADDR_WORD: 'ref.w',
    OP_ADDR_THREE: 'ref.3',

    OP_ADD: 'add',
    OP_SUB: 'sub',
    OP_MUL: 'mul',
    OP_DIV: 'div',
    OP_MOD: 'mod',
    OP_BITWISE_AND: 'bit.and',
    OP_BITWISE_OR: 'bit.or',
    OP_BITWISE_NOT: 'bit.not',

    OP_EQ: 'eq',
    OP_NE: 'ne',
    OP_GE: 'ge',
    OP_LE: 'le',
    OP_GT: 'gt',
    OP_LT: 'lt',

    OP_AND: 'and',
    OP_OR: 'or',
    OP_NOT: 'not',

    OP_PUSH_BYTE: 'push.b',
    OP_PUSH_WORD: 'push.w',
    OP_PUSH_THREE: 'push.3',

    OP_JZ: 'jz',
    OP_CASE: 'j_case',
    OP_JP: 'jp',
    OP_END: 'end',
    OP_POP_74: 'pop_74',
    OP_LINK_CHARACTER: 'link_char',

    OP_MASK: 'mask',
    OP_RANDOM: 'random',
    OP_RANDOM_BIT: 'random.bit',
    OP_COUNT: 'count',
    OP_GREATEST: 'set_greatest',
    OP_LEAST: 'set_least',
    OP_MP_COST: 'mp_cost',
    OP_SHIFT: 'shift',

    OP_WRITE: 'write',
    OP_POP: 'pop',
    OP_ATTACK: 'attack',
    OP_SAY: 'say',
    OP_COPY_UNIT: 'copy_unit',
    OP_LOADSAVE: 'load_save',
    OP_ELEMENTAL_DEFENCE: 'elemental_defence',

    OP_DEBUG: 'debug',
    OP_POP2_A1: 'pop2_a1'
}

args = {
    OP_READ_BIT: WordOp,
    OP_READ_BYTE: WordOp,
    OP_READ_WORD: WordOp,
    OP_READ_THREE: WordOp,

    OP_ADDR_BIT: WordOp,
    OP_ADDR_BYTE: WordOp,
    OP_ADDR_WORD: WordOp,
    OP_ADDR_THREE: WordOp,

    OP_PUSH_BYTE: ByteOp,
    OP_PUSH_WORD: WordOp,
    OP_PUSH_THREE: ThreeOp,

    OP_JZ: WordOp,
    OP_CASE: WordOp,
    OP_JP: WordOp,

    OP_SAY: StringOp,
    OP_DEBUG: StringOp
}


def disassemble(f: BinaryIO):
    i = 0
    while True:
        old = f.tell()
        code = f.read(1)
        if not len(code):
            break
        op = code[0]
        if op not in ops:
            print("Unknown opcode: %02x" % op)
            break
        mne = ops[op]
        if op in args:
            arg = args[op](f)
            print("%03x %s %s" % (i, mne, arg))
        else:
            print("%03x %s" % (i, mne))
        i += f.tell() - old


if __name__ == "__main__":
    raw = b'\x01\x00\x00\x60\x00\x71\x00\x0b\x72\x00\x10\x60\x01\x71\x00\x45\x12\x20\x70\x02\x20\xa0\x82\x90\x60\x20\x61\x02\x49\x92\x81\x01\x00\x20\x34\x52\x70\x00\x35\x12\x20\x70\x02\x20\xa0\x82\x90'
    f = BytesIO(raw)
    disassemble(f)
