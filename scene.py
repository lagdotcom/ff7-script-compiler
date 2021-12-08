from enum import Enum, IntFlag, auto
from gzip import decompress
from io import BytesIO
from os import altsep, stat
from smart_arg import arg_suite
from struct import unpack
from sys import argv
from typing import IO, Iterable, NamedTuple, Optional

from compiler import Compiler
from disassembler import NiceFormatter, ProudClodText
from strings import translate


def fixString(s: str):
    return translate(s.strip(b'\xff\0'))


class BattleLocation(Enum):
    Blank = 0
    BizzaroCentre = auto()
    Grassland = auto()
    MtNibel = auto()
    Forest = auto()
    Beach = auto()
    Desert = auto()
    Snow = auto()
    Swamp = auto()
    Sector1TrainStation = auto()
    Reactor1 = auto()
    Reactor1Core = auto()
    Reactor1Entrance = auto()
    Sector4Subway = auto()
    NibelCaves = auto()
    ShinraHQ = auto()
    MidgarSubway = auto()
    HojoLab = auto()
    ShinraElevator = auto()
    ShinraRoof = auto()
    MidgarHighway = auto()
    WutaiPagoda = auto()
    Church = auto()
    CoralValley = auto()
    MidgarSlums = auto()
    Sector4Corridors = auto()
    Sector4Gantries = auto()
    Sector7PillarStairway = auto()
    Sector7PillarTop = auto()
    Sector8 = auto()
    Sewers = auto()
    MythrilMines = auto()
    NorthernCraterPlatforms = auto()
    CorelMountainPath = auto()
    JunonBeach = auto()
    JunonCargoShip = auto()
    CorelPrison = auto()
    BattleSquare = auto()
    DaChaoStatue = auto()
    CidBackyard = auto()
    FinalDescent = auto()
    Reactor5Entrance = auto()
    TempleOfTheAncientsEscherRoom = auto()
    ShinraMansion = auto()
    JunonAirshipDock = auto()
    WhirlwindMaze = auto()
    JunonUnderwaterReactor = auto()
    GongagaReactor = auto()
    Gelnika = auto()
    TrainGraveyard = auto()
    IceCave = auto()
    SisterRay = auto()
    SisterRayBase = auto()
    ForgottenCityAltar = auto()
    NorthernCraterDescent = auto()
    NorthernCraterHatchery = auto()
    NorthernCraterWater = auto()
    SaferBattle = auto()
    KalmFlashback = auto()
    JunonUnderwaterPipe = auto()
    Blank_3C = auto()
    CorelRailwayCanyon = auto()
    WhirlwindMazeCrater = auto()
    CorelRailwayRollercoaster = auto()
    WoodenBridge = auto()
    DaChao = auto()
    FortCondor = auto()
    DirtWasteland = auto()
    BizzaroRight = auto()
    BizarroLeft = auto()
    JenovaSYNTHESIS = auto()
    CorelTrain = auto()
    CosmoCanyon = auto()
    CavernsOfTheGi = auto()
    NibelheimMansionBasement = auto()
    TempleOfTheAncientsDemonsGate = auto()
    TempleOfTheAncientsMuralRoom = auto()
    TempleOfTheAncientsClockPassage = auto()
    FinalBattle = auto()
    Jungle = auto()
    UltimateWeaponHighwind = auto()
    CorelReactor = auto()
    Unused_52 = auto()
    DonCorneoMansion = auto()
    EmeraldWeapon = auto()
    Reactor5 = auto()
    ShinraHQEscape = auto()
    UltimateWeaponGongagaReactor = auto()
    CorelPrisonDyne = auto()
    UltimateWeaponForest = auto()


class ElementIndex(Enum):
    Fire = 0
    Ice = auto()
    Bolt = auto()
    Earth = auto()
    Bio = auto()
    Gravity = auto()
    Water = auto()
    Wind = auto()
    Holy = auto()
    Health = auto()
    Cut = auto()
    Hit = auto()
    Punch = auto()
    Shoot = auto()
    Scream = auto()
    Hidden = auto()
    Death = 0x20
    NearDeath = auto()
    Sleep = auto()
    Poison = auto()
    Sadness = auto()
    Fury = auto()
    Confu = auto()
    Silence = auto()
    Haste = auto()
    Slow = auto()
    Stop = auto()
    Frog = auto()
    Small = auto()
    SlowNumb = auto()
    Petrify = auto()
    Regen = auto()
    Barrier = auto()
    MBarrier = auto()
    Reflect = auto()
    Dual = auto()
    Shield = auto()
    DeathSentence = auto()
    Manipulate = auto()
    Berserk = auto()
    Peerless = auto()
    Paralysis = auto()
    Darkness = auto()
    DualDrain = auto()
    DeathForce = auto()
    Resist = auto()
    LuckyGirl = auto()
    Imprisoned = auto()


class ElementFlags(IntFlag):
    NONE = 0
    Fire = 1
    Ice = auto()
    Bolt = auto()
    Earth = auto()
    Bio = auto()
    Gravity = auto()
    Water = auto()
    Wind = auto()
    Holy = auto()
    Health = auto()
    Cut = auto()
    Hit = auto()
    Punch = auto()
    Shoot = auto()
    Scream = auto()
    Hidden = auto()


class StatusEffect(IntFlag):
    NONE = 0
    Death = 1
    NearDeath = auto()
    Sleep = auto()
    Poison = auto()
    Sadness = auto()
    Fury = auto()
    Confu = auto()
    Silence = auto()
    Haste = auto()
    Slow = auto()
    Stop = auto()
    Frog = auto()
    Small = auto()
    SlowNumb = auto()
    Petrify = auto()
    Regen = auto()
    Barrier = auto()
    MBarrier = auto()
    Reflect = auto()
    Dual = auto()
    Shield = auto()
    DeathSentence = auto()
    Manipulate = auto()
    Berserk = auto()
    Peerless = auto()
    Paralysis = auto()
    Darkness = auto()
    DualDrain = auto()
    DeathForce = auto()
    Resist = auto()
    LuckyGirl = auto()
    Imprisoned = auto()


class ElementRate(Enum):
    Death = 0
    DoubleDamage = 2
    HalfDamage = 4
    Nullify = 5
    Absorb = 6
    FullCure = 7


class ItemDropSteal:
    drop: bool
    steal: bool
    rate: int

    def __init__(self, drop: bool, rate: int):
        self.drop = drop
        self.steal = not drop
        self.rate = rate

    def __repr__(self) -> str:
        if self.drop:
            return "Drop (%d/64)" % self.rate
        return "Steal (%d/64)" % self.rate


scriptNames = ['Initialize', 'Main', 'General Counter', 'Death Counter', 'Physical Counter', 'Magical Counter', 'Battle End',
               'Pre-Action Setup', 'Custom 1', 'Custom 2', 'Custom 3', 'Custom 4', 'Custom 5', 'Custom 6', 'Custom 7', 'Custom 8']


class AIData:
    def __init__(self, f: IO) -> None:
        start = f.tell()
        self.offsets = unpack('<hhhhhhhhhhhhhhhh', f.read(32))
        # TODO read scripts!!

    @property
    def present(self) -> str:
        scripts = []
        for i in range(16):
            if self.offsets[i] != -1:
                scripts.append(scriptNames[i])
        return ', '.join(scripts)


class Enemy:
    id: int
    name: str
    level: int
    speed: int
    luck: int
    evade: int
    strength: int
    defense: int
    magic: int
    magicDefense: int
    elements: dict[ElementIndex, ElementRate]
    animations: Iterable[int]
    attacks: Iterable[int]
    movements: Iterable[int]
    items: dict[int, ItemDropSteal]
    autoAttacks: Iterable[int]
    unknown9A: int
    mp: int
    ap: int
    morph: int
    backMultiplier: float
    hp: int
    exp: int
    gil: int
    immunity: StatusEffect
    unknownB4: int
    ai: AIData

    @property
    def elementalRates(self) -> str:
        rates = []
        for (e, r) in self.elements.items():
            rates.append('%s (%s)' % (e.name, r.name))
        return ', '.join(rates)

    def __repr__(self) -> str:
        return '\n'.join(["%s (#%04x), Level %d" % (self.name, self.id, self.level),
                          'Spd: %d  Luck: %d  Evade: %d  Str: %d  Def: %d  Mag: %d  MDf: %d' % (
                              self.speed, self.luck, self.evade, self.strength, self.defense, self.magic, self.magicDefense),
                          'HP: %d  MP: %d  EXP: %d  Gil: %d  AP: %d' % (self.hp, self.mp,
                                                                        self.exp, self.gil, self.ap),
                          "Items: %s" % self.items,
                          "Elements: %s" % self.elementalRates,
                          "Immunities: %s" % self.immunity,
                          'AI Scripts: %s' % self.ai.present])

    def read(self, f: IO):
        name, self.level, self.speed, self.luck, self.evade, self.strength, self.defense, self.magic, self.magicDefense = unpack(
            '<32sBBBBBBBB', f.read(40))
        elems = unpack('<bbbbbbbb', f.read(8))
        erates = unpack('<bbbbbbbb', f.read(8))
        self.animations = unpack('<bbbbbbbbbbbbbbbb', f.read(16))
        self.attacks = unpack('<hhhhhhhhhhhhhhhh', f.read(32))
        self.movements = unpack('<hhhhhhhhhhhhhhhh', f.read(32))
        irates = unpack('<BBBB', f.read(4))
        items = unpack('<hhhh', f.read(8))
        self.autoAttacks = unpack('<hhh', f.read(6))
        self.unknown9A, self.mp, self.ap, self.morph, mul, _pad, self.hp, self.exp, self.gil, immune, self.unknownB4 = unpack(
            '<HHHhBBIIIII', f.read(30))
        self.name = fixString(name)
        self.elements = {}
        for i in range(8):
            e, r = elems[i], erates[i]
            if e == -1:
                continue
            self.elements[ElementIndex(e)] = ElementRate(r)
        self.items = {}
        for i in range(4):
            id = items[i]
            if id == -1:
                continue
            drop = True
            rate = irates[i]
            if rate & 0x80:
                rate -= 0x80
                drop = False
            self.items[id] = ItemDropSteal(drop, rate)
        self.backMultiplier = mul / 8
        if immune == 0xffffffff:
            immune = 0
        else:
            immune = ~immune
        self.immunity = StatusEffect(immune)


class SetupFlags(IntFlag):
    pass


class SetupLayout(Enum):
    Normal = 0
    Preemptive = auto()
    Back = auto()
    Side = auto()
    Pincer = auto()
    Pincer2 = auto()
    Side2 = auto()
    Side3 = auto()
    NoChange = auto()


class Setup:
    location: BattleLocation
    continuation: Optional[int]
    escape: int
    nextArenaBattle: list[int]
    flags: SetupFlags
    layout: SetupLayout
    camera: int

    def __init__(self, f: IO):
        loc, cont, self.escape, _pad, arena1, arena2, arena3, arena4, flags, layout, self.camera = unpack(
            '<HhHHHHHHHBB', f.read(20))
        self.location = BattleLocation(loc)
        if cont != -1:
            self.continuation = cont
        else:
            self.continuation = None
        self.nextArenaBattle = [x for x in [
            arena1, arena2, arena3, arena4] if x != 999]
        self.flags = SetupFlags(flags)
        self.layout = SetupLayout(layout)

    def __repr__(self) -> str:
        return "Setup[%s, %s, %s, %s, %s, %s, %s]" % (self.location, self.continuation, self.escape, self.nextArenaBattle, self.flags, self.layout, self.camera)


class CameraPosition:
    def __init__(self, f: IO):
        self.pos = unpack('<HHH', f.read(6))
        self.ang = unpack('<HHH', f.read(6))

    def __repr__(self) -> str:
        pos = "(%d,%d,%d)" % self.pos
        ang = "(%d,%d,%d)" % self.ang
        return "%s@%s" % (pos, ang)


class CameraPlacement:
    def __init__(self, f: IO):
        self.primary = CameraPosition(f)
        self.secondary = CameraPosition(f)
        self.tertiary = CameraPosition(f)
        f.read(12)


class FormationEnemyFlags(IntFlag):
    Visible = 1
    Facing = 2
    Unknown = 4
    Targetable = 8
    Active = 16


class FormationEnemy:
    def __init__(self, f: IO):
        self.id, self.x, self.y, self.z, self.row, self.cover, flags = unpack(
            '<hhhhHHI', f.read(16))
        self.flags = FormationEnemyFlags(flags & 0x1f)

    def __repr__(self) -> str:
        return "Enemy[#%04x, (%d,%d,%d), %d, %d, %s]" % (self.id, self.x, self.y, self.z, self.row, self.cover, self.flags)


class Formation:
    def __init__(self, f: IO):
        self.enemies = [e for e in [FormationEnemy(
            f) for i in range(6)] if e.id != -1]

    def __repr__(self) -> str:
        return 'Formation:' + ''.join(['\n\t'+str(e) for e in self.enemies])


class TargetFlags(IntFlag):
    Self = 0
    Selection = 0x1
    Enemy = 0x2
    DefaultMultiple = 0x4
    Multiple = 0x8
    OneRow = 0x10
    ShortRange = 0x20
    AllRows = 0x40
    OneAtRandom = 0x80


standardFormulae = {
    0: 'No Damage',
    1: '(Power / 16) * (Stat + [(Level + Stat) / 32]^2) {SAD/SPLIT/BAR/VAR}',
    2: '(Power / 16) * ((Lvl + Stat) * 6) {SAD/SPLIT/BAR/VAR}',
    3: 'HP * (Power / 32)',
    4: 'MHP * (Power / 32)',
    5: '(Power * 22) + ((Level + Stat) * 6) {SPLIT/BAR/VAR}',
    6: 'Power * 20',
    7: 'Power / 32 {VAR}',
    8: 'Recovery',
    9: 'Throw',
    10: 'Coin',
}
specialFormulae = {
    0: "100% User's HP",
    8: 'Dice Roll x 100',
    9: 'Number of Escapes * 256',
    10: "Target's HP - 1",
    11: 'Number of hours on game clock * 100 + number of minutes in game clock',
    12:	"10 x Target's Kills",
    13:	"1111 x Target's Materia",
}
alteredFormulae = {
    0: "Damage * (1 + [User's Status Effects])",
    1: "Damage * (2 if Near-Death, 4 if in D.Sentence [can stack to 8], 1 if neither)",
    2: "Damage * (1 + Dead Allies)",
    3: "Power becomes average of all Targets' Levels",
    4: "Power becomes 1 + ( ( Power * 3 * HP ) / MHP )",
    5: "Power becomes 1 + ( ( Power * 3 * MP ) / MMP )",
    6: "Power becomes 1 + ( Power * [AP on Weapon / 10000] / 16 )",
    7: "Power becomes 10 + ( [Character's Kills / 128] * Power) / 16 )",
    8: "Power becomes 1 + ( [Limit Level * Limit Units / 16] * Power ) / 16",
}


def lookup(d: dict[int, str], i: int) -> str:
    if i in d:
        return d[i]
    return 'Unknown: %x' % i


def describeDamageCalculation(calc: int, acc: int) -> str:
    upper = calc >> 4
    lower = calc % 4
    if upper == 0 or upper == 3:
        return r'Physical, always hits >> ' + lookup(standardFormulae, lower)
    elif upper == 1:
        return 'Physical, %d%% hit rate, Allow Critical >> ' % acc + lookup(standardFormulae, lower)
    elif upper == 2:
        return 'Magical, %d%% hit rate >> ' % acc + lookup(standardFormulae, lower)
    elif upper == 4 or upper == 5:
        return r'Magical, always hits >> ' + lookup(standardFormulae, lower)
    elif upper == 6:
        return 'Physical, %d%% hit rate, Allow Critical >> ' % acc + lookup(specialFormulae, lower)
    elif upper == 7:
        return 'Magical, %d%% hit rate >> ' % acc + lookup(specialFormulae, lower)
    elif upper == 8:
        return 'Magical, only hits level mod %d >> ' % acc + lookup(standardFormulae, lower)
    elif upper == 9:
        return 'Magical, "Manipulate" accuracy >> ' + lookup(standardFormulae, lower)
    elif upper == 10:
        return describeDamageCalculation(0x11, acc) + ' >> ' + lookup(alteredFormulae, lower)
    elif upper == 11:
        return 'Physical, %d%% hit rate >> ' % acc + lookup(standardFormulae, lower)
    return 'Unknown: %02x' % calc


specialEffects = {
    0: '%d hit(s)',
    1: 'if enemies are immune, do Gunge Lance',
    2: 'summon Fat Chocobo, %d/255 chance',
    3: 'start main script %04x',
    4: 'cause back attack damage to target in row %d',
    5: 'end battle, no reward',
    6: 'steal (Level * 20) Gil from target',
    7: 'steal item from target',
    8: 'randomly select animation',
    9: 'if equal level, 8x damage',
    10: 'Master Fist',
    11: 'Powersoul',
    12: 'Princess Guard',
    13: 'Conformer',
    14: 'resurrect dead allies',
    15: 'Slots',
    16: 'Slots: Transform',
    17: 'remove from battle (dead)',
    18: 'remove from battle (escaped)',
    19: 'Tifa Slot Critical',
    20: 'Fury Brand',
    21: 'alter damage/defense by (100-%d) percent',
    22: 'alter my evasion by (%d-100) percent',
    23: 'alter my attack by (%d-100) percent',
    24: 'perform attack/item %04x',
    25: 'change rows',
    26: 'perform attack %04x on other row members',
    27: 'remove me from battle (escaped)',
    28: 'alter defense by (%d-100) percent',
    29: 'return from escaped',
    30: 'scale damage by current HP percentage',
    31: 'scale damage by current MP percentage',
    32: 'scale damage by AP on weapon',
    33: 'scale damage by kills',
    34: 'scale damage by current Limit percentage',
    35: 'receive no gil or items from target on death',
}


def describeSpecialEffect(effect: int, mod: int) -> str:
    if effect in specialEffects:
        s = specialEffects[effect]
        if '%' in s:
            return s % mod
        return s
    return 'Unknown: %x' % effect


class AttackCondition(Enum):
    PartyHP = 0
    PartyMP = 1
    PartyStatus = 2
    NONE = 255


class AttackFlags(IntFlag):
    NONE = 0
    DamageMP = 1
    Unknown02 = auto()
    Darkness = auto()
    Unused08 = auto()
    DrainHP = auto()
    DrainHPMP = auto()
    Unknown40 = auto()
    IgnoreStatusDefense = auto()
    MissIfNotDead = auto()
    Reflectable = auto()
    IgnoreDefense = auto()
    DoNotRetargetDead = auto()
    Unused1000 = auto()
    AlwaysCritical = auto()
    Unused4000 = auto()
    Unused8000 = auto()


class Attack:
    id: int
    name: str

    def __init__(self, f: IO) -> None:
        self.accuracy, self.impactEffect, self.hurtAction, self.unknown03, self.cost, self.impactSound, self.cameraSingle, self.cameraMultiple, target, self.effectId, self.calculation, self.power, condition, self.statusChange, self.special, self.specialMod, self.status, element, flags = unpack(
            '<BBBBHHHHBBBBBBbbIHH', f.read(28))
        self.target = TargetFlags(target)
        self.condition = AttackCondition(condition)
        self.element = ElementFlags(element)
        if flags == 0xffff:
            flags = 0
        else:
            flags = ~flags
        self.flags = AttackFlags(flags)

    @property
    def statusEffects(self) -> str:
        if self.status == 0xffffffff:
            return 'No Status Effects'
        chance = self.statusChange & 0x3f
        cure = self.statusChange & 0x40
        toggle = self.statusChange & 0x80
        effect = 'Inflict'
        if toggle:
            effect = 'Toggle'
        elif cure:
            effect = 'Cure'
        return '%s (%d/63): %s' % (effect, chance, StatusEffect(self.status))

    def __repr__(self) -> str:
        lines = ['%s (#%04x), %s' % (self.name, self.id, self.target), 'MP: %d  Power: %d  %s  %s  %s' % (
            self.cost, self.power, self.condition, self.element, self.flags), describeDamageCalculation(self.calculation, self.accuracy)]
        if self.special != -1:
            lines.append(describeSpecialEffect(self.special, self.specialMod))
        if self.status != 0xffffffff:
            lines.append(self.statusEffects)
        return '\n'.join(lines)


class SceneData:
    enemies: list[Enemy]
    setups: list[Setup]
    cameras: list[CameraPlacement]
    formations: list[Formation]
    attacks: list[Attack]
    aiOffsets: Iterable[int]
    ai: bytes

    def __init__(self, f: IO):
        self.enemies = [Enemy(), Enemy(), Enemy()]
        self.readIDs(f)
        self.readSetups(f)
        self.readCameras(f)
        self.readFormations(f)
        self.readEnemies(f)
        self.readAttacks(f)
        self.readFormationAI(f)
        self.readEnemyAI(f)

    def readIDs(self, f: IO):
        ida, idb, idc, _padding = unpack('<hhhh', f.read(8))
        self.enemies[0].id = ida
        self.enemies[1].id = idb
        self.enemies[2].id = idc

    def readSetups(self, f: IO):
        self.setups = [Setup(f) for i in range(4)]

    def readCameras(self, f: IO):
        self.cameras = [CameraPlacement(f) for i in range(4)]

    def readFormations(self, f: IO):
        self.formations = [Formation(f) for i in range(4)]

    def readEnemies(self, f: IO):
        for enemy in self.enemies:
            enemy.read(f)

    def readAttacks(self, f: IO):
        self.attacks = [Attack(f) for i in range(32)]
        ids = unpack('<hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh', f.read(64))
        for i in range(32):
            self.attacks[i].id = ids[i]
            self.attacks[i].name = fixString(f.read(32))

    def readFormationAI(self, f: IO):
        self.aiOffsets = unpack('<hhhh', f.read(8))
        self.ai = f.read(504)

    def readEnemyAI(self, f: IO):
        start = f.tell()    # should always be 0xE80
        offsets = unpack('<hhh', f.read(6))
        for i in range(3):
            o = offsets[i]
            if o == -1:
                continue
            f.seek(start + o)
            self.enemies[i].ai = AIData(f)


class SceneFile(NamedTuple):
    blockStart: int
    start: int
    end: int

    @property
    def size(self):
        return self.end - self.start


class SceneBlock:
    files: list[SceneFile]

    def __init__(self, f: IO):
        self.start = f.tell()
        offsets = unpack('<iiiiiiiiiiiiiiii', f.read(64))
        last = False
        self.files = []
        for i in range(16):
            o = offsets[i]
            if i < 15:
                e = offsets[i+1]
                if e == -1:
                    e = 0x800
                    last = True
            else:
                e = 0x800
            self.files.append(SceneFile(self.start, o*4, e*4))
            if last:
                break


class SceneBin:
    blocks: list[SceneBlock]

    def __init__(self, fn: str):
        self.f = open(fn, 'rb')
        self.size = stat(fn).st_size
        self.readBlocks()

    def readBlocks(self):
        self.blocks = []
        count = self.size // 0x2000
        for i in range(count):
            self.f.seek(i * 0x2000)
            self.blocks.append(SceneBlock(self.f))

    def getFileContents(self, i: int):
        block = i // 16
        file = i % 16
        ref = self.blocks[block].files[file]
        self.f.seek(ref.blockStart + ref.start)
        return decompress(self.f.read(ref.size).strip(bytes([255])))

    def dump(self, i: int, fn: str):
        open(fn, 'wb').write(self.getFileContents(i))


@arg_suite
class Args(NamedTuple):
    src: str  # scene file
    enemy: str  # enemy ID
    ai: str  # AI source file


def process(a: Args):
    aiSource = open(a.ai, 'r').read()
    c = Compiler()
    success = c.compile(aiSource)
    for chunk in c.chunks:
        print(chunk.name + ':')
        buf = BytesIO(chunk.code)
        ProudClodText(buf)
        print()
    if not success:
        return

    f = open(a.src, 'rb')
    dat = SceneData(f)
    # print("=== SETUPS")
    # for setup in dat.setups:
    #     print(setup)
    # print("=== FORMATIONS")
    # for formation in dat.formations:
    #     print(formation)
    # print("=== ATTACKS")
    # for attack in dat.attacks:
    #     if attack.id == -1:
    #         continue
    #     print(attack)
    #     print()
    # print("=== ENEMIES")
    # for enemy in dat.enemies:
    #     if enemy.id == -1:
    #         continue
    #     print(enemy)
    #     print()


if __name__ == "__main__":
    args = Args.__from_argv__(argv[1:])
    process(args)
