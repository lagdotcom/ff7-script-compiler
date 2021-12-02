from io import BytesIO
import sys
from smart_arg import arg_suite
from typing import NamedTuple

from compiler import Compiler
from disassembler import disassemble


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
        disassemble(buf)


if __name__ == '__main__':
    args = Args.__from_argv__(sys.argv[1:])
    process(args)
