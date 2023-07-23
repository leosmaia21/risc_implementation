from enum import Enum
import struct
import glob
from elftools.elf.elffile import ELFFile

regfile = [0]*33
PC = 32

class Regfile():
    def __init__(self):
        self.regs = [0]*33
    def __getitem__(self, key):
        return self.regs[key]
    def __setitem__(self, key, value):
        if key == 0:
            return
        self.regs[key] = value & 0xFFFFFFFF

regfile = Regfile()


class Ops(Enum):
    LUI = 0b110111
    LOAD = 0b0000011
    STORE = 0b0100011

    AUIPC = 0b0010111
    BRANCH = 0b1100011
    JAL = 0b1101111
    JALR = 0b1100111
    FENCE = 0b0001111


    IMM = 0b0010011
    OP = 0b0110011

    SYSTEM = 0b1110011
    MISC = 0b0001111


class Funct3(Enum):
    ADD = ADDI = SUB = 0b000
    SLLI = 0b001
    SLT = SLTI = 0b010
    SLTU = SLTIU = 0b100
    XOR = XORI = 0b10
    OR = ORI = 0b110
    AND = ANDI = 0b111
    SRL = SRLI = SRA = SRAI = 0b101


# 4k at 0x80000000
memory = b'\x00'*0x10000


def getBits(ins, s, e):
    return (ins >> e) & ((1 << (s - e + 1)) - 1)


def ws(dat, addr):
    global memory
    addr -= 0x80000000
    memory = memory[:addr] + dat + memory[addr+len(dat):]


def r32(addr):
    addr -= 0x80000000
    assert addr >= 0 and addr < len(memory)
    # little endian
    return struct.unpack("<I", memory[addr:addr+4])[0]


def dump():
    pp = []
    for i in range(32):
        if i != 0 and i % 8 == 0:
            pp += "\n"
        pp += " %3s: %08x" % ("x%d" % i, regfile[i])
    pp += "\n PC: %08x" % regfile[PC]
    print(''.join(pp))


def step():
    ins = r32(regfile[PC])
    opcode = Ops(getBits(ins, 6, 0))
    print("%x %8x %r" % (regfile[PC], ins, opcode))
    if opcode == Ops.JAL:
        # J type instruction
        rd = getBits(ins, 11, 7)
        offset = getBits(ins, 31, 31) << 20 | getBits(ins, 30, 21) << 1 | getBits(ins, 21, 20) << 11 | getBits(ins, 19, 12) << 12
        regfile[PC] += offset
        return True
    elif opcode == Ops.JALR:
        rd = getBits(ins, 11, 7)
        rs1 = getBits(ins, 19, 15)
        imm = getBits(ins, 31,20)
        pass
    elif opcode == Ops.AUIPC:
        # U type instruction
        rd = getBits(ins, 11, 7)
        imm = getBits(ins, 31, 20)
        regfile[PC] = regfile[PC] + imm
    elif opcode == Ops.OP:
        rd = getBits(ins, 11, 7)
        rs1 = getBits(ins, 19, 15)
        rs2 = getBits(ins, 24, 20)
        funct3 = Funct3(getBits(ins, 14, 12)).value
        funct7 = getBits(ins, 31, 25)
        if funct3 == Funct3.ADD.value:
            regfile[rd] = regfile[rs1] + regfile[rs2]
        else:
            dump()
            raise Exception("write funct3 %r" % funct3)
    elif opcode == Ops.IMM:
        # I type instruction
        rd = getBits(ins, 11, 7)
        rs1 = getBits(ins, 19, 15)
        funct3 = getBits(ins, 14, 12)
        imm = getBits(ins, 31, 20)
        if funct3 == Funct3.ADDI.value:
            regfile[rd] = regfile[rs1] + imm
        elif funct3 == Funct3.SLLI.value:
            regfile[rd] = regfile[rs1] << imm
        else:
            dump()
            raise Exception("write funct3 %r" % funct3)
            # return True
    elif opcode == Ops.SYSTEM:
        pass
    else:
        raise Exception("write op %r" % opcode)
        dump()

    regfile[PC] += 4
    return True


if __name__ == "__main__":
    for x in glob.glob("riscv-tests/isa/rv32ui-v-add"):
        if x.endswith('.dump'):
            continue
        with open(x, 'rb') as f:
            print("teste:", x)
            e = ELFFile(f)
            for s in e.iter_segments():
                ws(s.data(), s.header.p_paddr)
            regfile[PC] = 0x80000000
            while step():
                pass
            # e.
        exit()
