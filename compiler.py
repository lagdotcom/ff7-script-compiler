
from typing import Callable, NamedTuple, Optional
from enums import Precedence, Size
from ops import Op
from parser import Parser
from scanner import Token, TokenType, Scanner
from tools import asNumber, hexBytes, splitString, splitThree, splitWord


class Constant(NamedTuple):
    name: str
    size: Size
    value: int

    def __repr__(self) -> str:
        return "const %s %s = %x" % (self.size.name, self.name, self.value)


class Variable(NamedTuple):
    name: str
    size: Size
    addr: int

    def __repr__(self) -> str:
        return "var %s %s at %x" % (self.size.name, self.name, self.addr)


class OpFunction(NamedTuple):
    name: str
    perform: any  # Callable[[Compiler], None]

    def __repr__(self) -> str:
        return "%s(...)" % self.name


class Chunk:
    name: str
    code: bytes
    line: int
    lines: list[int]

    def __init__(self, name: str):
        self.name = name
        self.code = bytes()
        self.line = 0
        self.lines = []

    def raw(self, *b: int) -> int:
        dst = bytes(b)
        self.code += dst
        self.lines += [self.line] * len(dst)
        #print('wrote:', hexBytes(dst))
        return len(dst)

    def read(self, var: Constant | Variable) -> int:
        if isinstance(var, Constant):
            value = var.value
        else:
            value = var.addr
        if var.size == Size.BIT:
            return self.readBit(value)
        elif var.size == Size.BYTE:
            return self.readByte(value)
        elif var.size == Size.WORD:
            return self.readWord(value)
        elif var.size == Size.TRIPLE:
            return self.readThree(value)
        return 0

    def readBit(self, addr: int) -> int:
        return self.raw(Op.READ_BIT.value, *splitWord(addr))

    def readByte(self, addr: int) -> int:
        return self.raw(Op.READ_BYTE.value, *splitWord(addr))

    def readWord(self, addr: int) -> int:
        return self.raw(Op.READ_WORD.value, *splitWord(addr))

    def readThree(self, addr: int) -> int:
        return self.raw(Op.READ_THREE.value, *splitWord(addr))

    def ref(self, var: Constant | Variable) -> int:
        if isinstance(var, Constant):
            value = var.value
        else:
            value = var.addr
        if var.size == Size.BIT:
            return self.refBit(value)
        elif var.size == Size.BYTE:
            return self.refByte(value)
        elif var.size == Size.WORD:
            return self.refWord(value)
        elif var.size == Size.TRIPLE:
            return self.refThree(value)
        return 0

    def refBit(self, addr: int) -> int:
        return self.raw(Op.ADDR_BIT.value, *splitWord(addr))

    def refByte(self, addr: int) -> int:
        return self.raw(Op.ADDR_BYTE.value, *splitWord(addr))

    def refWord(self, addr: int) -> int:
        return self.raw(Op.ADDR_WORD.value, *splitWord(addr))

    def refThree(self, addr: int) -> int:
        return self.raw(Op.ADDR_THREE.value, *splitWord(addr))

    def add(self) -> int:
        return self.raw(Op.ADD.value)

    def sub(self) -> int:
        return self.raw(Op.SUB.value)

    def mul(self) -> int:
        return self.raw(Op.MUL.value)

    def div(self) -> int:
        return self.raw(Op.DIV.value)

    def mod(self) -> int:
        return self.raw(Op.MOD.value)

    def bitAnd(self) -> int:
        return self.raw(Op.BITWISE_AND.value)

    def bitOr(self) -> int:
        return self.raw(Op.BITWISE_OR.value)

    def bitNot(self) -> int:
        return self.raw(Op.BITWISE_NOT.value)

    def eq(self) -> int:
        return self.raw(Op.EQ.value)

    def ne(self) -> int:
        return self.raw(Op.NE.value)

    def ge(self) -> int:
        return self.raw(Op.GE.value)

    def le(self) -> int:
        return self.raw(Op.LE.value)

    def gt(self) -> int:
        return self.raw(Op.GT.value)

    def lt(self) -> int:
        return self.raw(Op.LT.value)

    def logAnd(self) -> int:
        return self.raw(Op.AND.value)

    def logOr(self) -> int:
        return self.raw(Op.OR.value)

    def logNot(self) -> int:
        return self.raw(Op.NOT.value)

    def push(self, value: int) -> int:
        if value <= 0xff:
            return self.pushByte(value)
        elif value <= 0xffff:
            return self.pushWord(value)
        elif value <= 0xffffff:
            return self.pushThree(value)
        else:
            raise Exception('push too big: %x' % value)

    def pushByte(self, value: int) -> int:
        return self.raw(Op.PUSH_BYTE.value, value)

    def pushWord(self, value: int) -> int:
        return self.raw(Op.PUSH_WORD.value, *splitWord(value))

    def pushThree(self, value: int) -> int:
        return self.raw(Op.PUSH_THREE.value, *splitThree(value))

    def jz(self, addr: int) -> int:
        return self.raw(Op.JZ.value, *splitWord(addr))

    def case(self, addr: int) -> int:
        return self.raw(Op.CASE.value, *splitWord(addr))

    def jp(self, addr: int) -> int:
        return self.raw(Op.JP.value, *splitWord(addr))

    def end(self) -> int:
        return self.raw(Op.END.value)

    def linkCharacter(self) -> int:
        return self.raw(Op.LINK_CHARACTER.value)

    def mask(self) -> int:
        return self.raw(Op.MASK.value)

    def random(self) -> int:
        return self.raw(Op.RANDOM.value)

    def randomBit(self) -> int:
        return self.raw(Op.RANDOM_BIT.value)

    def count(self) -> int:
        return self.raw(Op.COUNT.value)

    def first(self) -> int:
        return self.raw(Op.FIRST.value)

    def greatest(self) -> int:
        return self.raw(Op.GREATEST.value)

    def least(self) -> int:
        return self.raw(Op.LEAST.value)

    def mpCost(self) -> int:
        return self.raw(Op.MP_COST.value)

    def shift(self) -> int:
        return self.raw(Op.SHIFT.value)

    def write(self) -> int:
        return self.raw(Op.WRITE.value)

    def writeMask(self) -> int:
        return self.raw(Op.WRITE_MASK.value)

    def pop(self) -> int:
        return self.raw(Op.POP.value)

    def attack(self) -> int:
        return self.raw(Op.ATTACK.value)

    def say(self, message: str) -> int:
        return self.raw(Op.SAY.value, *splitString(message))

    def copyUnit(self) -> int:
        return self.raw(Op.COPY_UNIT.value)

    def load(self) -> int:
        return self.raw(Op.LOADSAVE.value)

    def save(self) -> int:
        return self.raw(Op.LOADSAVE.value)

    def elementalDefence(self) -> int:
        return self.raw(Op.ELEMENTAL_DEFENCE.value)

    def debug(self, message: str) -> int:
        return self.raw(Op.DEBUG.value, *splitString(message))


class Compiler:
    chunks: list[Chunk]
    compiling: Chunk
    scanner: Scanner
    parser: Parser
    declarations: dict[str, Constant | OpFunction | Variable]
    scopeDepth: int

    def __init__(self):
        self.chunks = []
        self.declarations = startingEnvironment.copy()
        self.scopeDepth = 0

    @property
    def current(self):
        return self.parser.current

    @property
    def previous(self):
        return self.parser.previous

    def declare(self, thing: Constant | Variable):
        if thing.name in self.declarations:
            self.parser.error("Variable redeclaration: %s" % thing.name)
            return
        self.declarations[thing.name] = thing
        print(thing)

    def chunk(self, name: str):
        ch = Chunk(name)
        self.chunks.append(ch)
        self.compiling = ch

    def advance(self):
        self.parser.previous = self.parser.current

        while True:
            self.parser.current = self.scanner.token()
            if self.parser.current.type != TokenType.ERROR:
                break

            self.parser.errorAtCurrent(self.parser.current.value)

    def consume(self, type: TokenType, message: str):
        if self.current.type == type:
            self.advance()
            return
        self.parser.errorAtCurrent(message)

    def check(self, type: TokenType) -> bool:
        return self.current.type == type

    def match(self, type: TokenType) -> bool:
        if not self.check(type):
            return False
        self.advance()
        return True

    def parsePrecedence(self, p: Precedence):
        self.advance()

        prefix = getRule(self.previous.type).prefix
        if not prefix:
            self.parser.error("Expect expression.")
            return

        canAssign = p.value <= Precedence.ASSIGNMENT.value
        prefix(self, canAssign)
        if canAssign and self.match(TokenType.EQUAL):
            self.parser.error("Invalid assignment target.")
            return

        while p.value <= getRule(self.current.type).precedence.value:
            self.advance()
            infix = getRule(self.previous.type).infix
            if infix:
                infix(self, canAssign)

    def expression(self):
        self.parsePrecedence(Precedence.ASSIGNMENT)

    def expressionStatement(self):
        self.expression()
        self.consume(TokenType.SEMICOLON, "Expect ';' after expression.")

    def parseVariable(self, message: str):
        self.consume(TokenType.IDENTIFIER, message)
        return self.previous.value

    def namedVariable(self, name: Token, canAssign: bool):
        #print('namedVariable', name, canAssign)
        if name.value not in self.declarations:
            self.parser.error("Undefined variable: %s" % name.value)
            return
        var = self.declarations[name.value]

        if isinstance(var, OpFunction):
            var.perform(self)
            return

        if canAssign and self.match(TokenType.EQUAL):
            self.compiling.ref(var)
            self.expression()
            self.compiling.write()
        else:
            self.compiling.read(var)

    def parseSize(self):
        if self.match(TokenType.BIT):
            return Size.BIT
        elif self.match(TokenType.BYTE):
            return Size.BYTE
        elif self.match(TokenType.WORD):
            return Size.WORD
        elif self.match(TokenType.TRIPLE):
            return Size.TRIPLE
        self.parser.errorAtCurrent("Expect bit/byte/word/triple.")

    def constDeclaration(self):
        size = self.parseSize()
        name = self.parseVariable("Expect variable name.")
        self.consume(TokenType.EQUAL, "Expect equals sign.")
        self.consume(TokenType.NUMBER, "Expect constant value.")
        num = self.previous
        self.consume(TokenType.SEMICOLON,
                     "Expect ';' after const declaration.")
        self.declare(Constant(name, size, asNumber(num.value)))

    def varDeclaration(self):
        size = self.parseSize()
        name = self.parseVariable("Expect variable name.")
        self.consume(TokenType.AT, "Expect 'at' after variable name.")
        self.consume(TokenType.NUMBER, "Expect constant value.")
        addr = self.previous
        self.consume(TokenType.SEMICOLON,
                     "Expect ';' after const declaration.")
        self.declare(Variable(name, size, asNumber(addr.value)))

    def scriptDeclaration(self):
        name = self.parseVariable("Expect script name.")
        self.chunk(name)

    def beginScope(self):
        self.scopeDepth += 1

    def endScope(self):
        self.scopeDepth -= 1
        if self.scopeDepth == 0:
            self.compiling.end()

    def block(self):
        while not self.check(TokenType.RIGHT_BRACE) and not self.check(TokenType.EOF):
            self.declaration()
        self.consume(TokenType.RIGHT_BRACE, "Expect '}' after block.")

    def statement(self):
        if self.match(TokenType.CONST):
            self.constDeclaration()
        elif self.match(TokenType.VAR):
            self.varDeclaration()
        elif self.match(TokenType.SCRIPT):
            self.scriptDeclaration()
        elif self.match(TokenType.LEFT_BRACE):
            self.beginScope()
            self.block()
            self.endScope()
        else:
            self.expressionStatement()

    def declaration(self):
        self.statement()
        if self.parser.panicMode:
            self.synchronize()

    def synchronize(self):
        self.parser.panicMode = False

        while self.current.type != TokenType.EOF:
            if self.previous.type == TokenType.SEMICOLON:
                return
            if self.current.type in [TokenType.SCRIPT, TokenType.IF, TokenType.CONST, TokenType.VAR]:
                return
            self.advance()

    def compile(self, code: str) -> bool:
        self.scanner = Scanner(code)
        self.parser = Parser()
        self.advance()

        while not self.match(TokenType.EOF):
            self.declaration()

        self.consume(TokenType.EOF, "Expect end of expression.")
        return not self.parser.hadError


ParseFn = Callable[[Compiler, bool], None]


class ParseRule(NamedTuple):
    prefix: Optional[ParseFn]
    infix: Optional[ParseFn]
    precedence: Precedence


def number(self: Compiler, canAssign: bool):
    value = asNumber(self.previous.value)
    self.compiling.push(value)


def grouping(self: Compiler, canAssign: bool):
    self.expression()
    self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")


def unary(self: Compiler, canAssign: bool):
    type = self.previous.type

    self.parsePrecedence(Precedence.UNARY)

    if type == TokenType.BIT_NOT:
        self.compiling.bitNot()
    elif type == TokenType.NOT:
        self.compiling.logNot()


def binary(self: Compiler, canAssign: bool):
    operatorType = self.previous.type
    rule = getRule(operatorType)
    self.parsePrecedence(Precedence(rule.precedence.value + 1))

    if operatorType == TokenType.PLUS:
        self.compiling.add()
    elif operatorType == TokenType.MINUS:
        self.compiling.sub()
    elif operatorType == TokenType.TIMES:
        self.compiling.mul()
    elif operatorType == TokenType.DIVIDE:
        self.compiling.div()
    elif operatorType == TokenType.MODULO:
        self.compiling.mod()
    elif operatorType == TokenType.BIT_AND:
        self.compiling.bitAnd()
    elif operatorType == TokenType.BIT_OR:
        self.compiling.bitOr()
    elif operatorType == TokenType.NOT_EQUAL:
        self.compiling.ne()
    elif operatorType == TokenType.EQUAL_EQUAL:
        self.compiling.eq()
    elif operatorType == TokenType.MORE:
        self.compiling.gt()
    elif operatorType == TokenType.MORE_EQUAL:
        self.compiling.ge()
    elif operatorType == TokenType.LESS:
        self.compiling.lt()
    elif operatorType == TokenType.LESS_EQUAL:
        self.compiling.le()


def variable(self: Compiler, canAssign: bool):
    self.namedVariable(self.previous, canAssign)


defaultRule = ParseRule(None, None, Precedence.NONE)
rules = {
    TokenType.LEFT_PAREN: ParseRule(grouping, None, Precedence.NONE),
    TokenType.MINUS: ParseRule(unary, binary, Precedence.TERM),
    TokenType.PLUS: ParseRule(None, binary, Precedence.TERM),
    TokenType.DIVIDE: ParseRule(None, binary, Precedence.FACTOR),
    TokenType.TIMES: ParseRule(None, binary, Precedence.FACTOR),
    TokenType.MODULO: ParseRule(None, binary, Precedence.FACTOR),
    TokenType.NUMBER: ParseRule(number, None, Precedence.NONE),
    TokenType.BIT_NOT: ParseRule(unary, None, Precedence.NONE),
    TokenType.NOT: ParseRule(unary, None, Precedence.NONE),
    TokenType.NOT_EQUAL: ParseRule(None, binary, Precedence.EQUALITY),
    TokenType.EQUAL_EQUAL: ParseRule(None, binary, Precedence.EQUALITY),
    TokenType.MORE: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.MORE_EQUAL: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.LESS: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.LESS_EQUAL: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.IDENTIFIER: ParseRule(variable, None, Precedence.NONE)
}


def getRule(type: TokenType) -> ParseRule:
    if type in rules:
        return rules[type]
    return defaultRule


def doRandom(self: Compiler):
    self.compiling.random()
    # TODO this sucks
    self.consume(TokenType.LEFT_PAREN, "Expect '(' after random.")
    self.consume(TokenType.RIGHT_PAREN, "Expect ')' after random(.")


startingEnvironment = {
    "Random": OpFunction("Random", doRandom)
}
