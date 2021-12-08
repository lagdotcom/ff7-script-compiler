from io import BytesIO
from sys import argv
from smart_arg import arg_suite
from typing import NamedTuple

from compiler import Compiler
from disassembler import ProudClodBinary


@arg_suite
class Args(NamedTuple):
    src: str  # source file


def process(a: Args):
    c = Compiler()
    raw = open(a.src, 'r').read()
    c.compile(raw)
    for chunk in c.chunks:
        print("==", chunk.name)
        buf = BytesIO(chunk.code)
        ProudClodBinary(buf)
        print()


if __name__ == '__main__':
    args = Args.__from_argv__(argv[1:])
    process(args)
