from enum import Enum, auto


class Size(Enum):
    BIT = 0
    BYTE = 1
    WORD = 2
    TRIPLE = 3


class Precedence(Enum):
    NONE = auto()
    ASSIGNMENT = auto()
    OR = auto()
    AND = auto()
    EQUALITY = auto()
    COMPARISON = auto()
    TERM = auto()
    FACTOR = auto()
    UNARY = auto()
    CALL = auto()
    PRIMARY = auto()
