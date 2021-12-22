from enum import Enum, auto
from io import BytesIO
from sys import argv, exit
from smart_arg import arg_suite
from typing import NamedTuple, Optional

from compiler import Compiler
from disassembler import NiceFormatter, ProudClodBinary, ProudClodText
from scene import SceneBin, SceneData, convertToAIData


class Formatter(Enum):
    Nice = auto()
    PCBin = auto()
    PCText = auto()


@arg_suite
class Args(NamedTuple):
    bin: Optional[str] = None  # scene.bin file
    scene: Optional[int] = None  # scene index
    src: Optional[str] = None  # scene file
    enemy: Optional[str] = None  # enemy ID
    ai: Optional[str] = None  # AI source file
    dumpScene: bool = False  # dump all scene data
    dumpEnemy: bool = False  # dump chosen enemy data
    dumpAI: bool = False  # dump chosen enemy AI
    formatter: Formatter = Formatter.Nice  # AI dump formatter
    showCompiled: bool = False  # dump compiled AI
    replaceAI: bool = False  # replace enemy AI


def die(err: str):
    print('***', err)
    exit(1)


def process(a: Args):
    if a.formatter == Formatter.Nice:
        fmt = NiceFormatter
    elif a.formatter == Formatter.PCBin:
        fmt = ProudClodBinary
    elif a.formatter == Formatter.PCText:
        fmt = ProudClodText

    comp = None
    if a.ai:
        aiSource = open(a.ai, 'r').read()
        comp = Compiler()
        success = comp.compile(aiSource)
        if a.showCompiled:
            for chunk in comp.chunks:
                print(chunk.name + ':')
                buf = BytesIO(chunk.code)
                fmt(buf)
                print()
        if not success:
            return die("Error while compiling, exiting early")
        else:
            print("*** Compiled", a.ai)

    bin = None
    dat = None
    en = None

    if a.bin:
        bin = SceneBin(a.bin)

        if a.scene:
            buf = BytesIO(bin.getFileContents(a.scene))
            dat = SceneData(buf)

    if a.src:
        f = open(a.src, 'rb')
        dat = SceneData(f)
        print("*** Loaded", a.src)

    if a.dumpScene:
        if not dat:
            return die("--dumpScene requires --bin BIN --scene NUM or --src")

        print("=== SETUPS")
        for setup in dat.setups:
            print(setup)
        print("=== FORMATIONS")
        for formation in dat.formations:
            print(formation)
        print("=== ATTACKS")
        for attack in dat.attacks:
            if attack.id == -1:
                continue
            print(attack)
            print()
        print("=== ENEMIES")
        for enemy in dat.enemies:
            if enemy.id == -1:
                continue
            print(enemy)
            print()

    if a.enemy:
        if not dat:
            return die("--enemy requires --bin BIN --scene NUM or --src")

        if a.enemy[:2] == '0x':
            want = int(a.enemy, 16)
        else:
            want = int(a.enemy, 10)

        for enemy in dat.enemies:
            if enemy.id == want:
                en = enemy
                break

        if not en:
            return die("Cannot find enemy of ID: %d" % want)

        if a.dumpEnemy:
            print(en)

        if a.dumpAI:
            # TODO
            buf = BytesIO(en.ai.src)
            fmt(buf)

    if a.replaceAI:
        if not comp or not dat or not en:
            return die("Require (--bin BIN --scene NUM or --src SCENE) --ai SRC --enemy ID")

        ai = convertToAIData(comp)
        print('New AI has:', ai.present)

        en.ai = ai
        ofn = a.src + '.tmp'
        dat.save(ofn)
        print('Wrote:', ofn)


if __name__ == "__main__":
    args = Args.__from_argv__(argv[1:])
    process(args)
