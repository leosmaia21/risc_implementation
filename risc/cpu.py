import struct
from enum import Enum


codeMemory = b'\x00'*0x20
dataMemory = b'\x00'*0x20

regfile = [0]*33
PC = 32


class Ops(Enum):
    LW  = 0x00
    SW  = 0x01
    ADD = 0x02
    SUB = 0x03
    INV = 0x04
    LSL = 0x05
    LSR = 0x06
    AND = 0x07
    OR  = 0x08
    SLT = 0x09

    BEQ = 0x0B
    BNE = 0x0C
    J   = 0x0D

def wordstore(memory, data, addr):
    global codeMemory
    global dataMemory
    if (type(data) == int):
        data = data.to_bytes(2, byteorder='big')
    if memory == "code":
        codeMemory = codeMemory[:addr] + data + codeMemory[addr+len(data):]
    elif memory == "data":
        dataMemory = dataMemory[:addr] + data + dataMemory[addr+len(data):]

def dump():
    pp = []
    for i in range(16):
        if i != 0 and i % 8 == 0:
            pp += "\n"
        pp += " %3s: %08x" % ("x%d" % i, regfile[i])
    pp += "\n PC: %08x" % regfile[PC]
    print(''.join(pp))

def rs(addr):
    ret = codeMemory[addr:addr+2]
    return int.from_bytes(ret, byteorder='big')

def instruction():
    ins = rs(regfile[PC])
    if ins == 0:
        print("halt")
        exit(0)
    op = ins >> 12
    rs1 = ins >> 9 & 0x07
    rs2 = ins >> 6 & 0x07
    ws = ins >> 3 & 0x07
    offset = ins & 0x3F
    if op == Ops.LW.value:
        print("LW")
        regfile[ws] = dataMemory[regfile[rs1] + offset: regfile[rs1] + offset + 2]
        regfile[ws] = int.from_bytes(regfile[ws], byteorder='big')
    elif op == Ops.SW.value:
        print("SW")
        dest = regfile[rs1] + offset
        wordstore("data", regfile[rs2], dest)
    elif op == Ops.ADD.value:
        print("ADD")
        regfile[ws] = regfile[rs1] + regfile[rs2]
    elif op == Ops.SUB.value:
        print("SUB")
        regfile[ws] = regfile[rs1] - regfile[rs2]
    elif op == Ops.INV.value:
        print("INV")
        regfile[ws] = ~regfile[rs1]
    elif op == Ops.LSL.value:
        print("LSL")
        regfile[ws] = regfile[rs1] << regfile[rs2]
    elif op == Ops.LSR.value:
        print("LSR")
        regfile[ws] = regfile[rs1] >> regfile[rs2]
    elif op == Ops.AND.value:
        print("AND")
        regfile[ws] = regfile[rs1] & regfile[rs2]
    elif op == Ops.OR.value:
        print("OR")
        regfile[ws] = regfile[rs1] | regfile[rs2]
    elif op == Ops.SLT.value:
        print("SLT")
        regfile[ws] = 1 if regfile[rs1] < regfile[rs2] else 0
    elif op == Ops.BEQ.value:
        print("BEQ")
        if regfile[rs1] == regfile[rs2]:
            regfile[PC] += 2 + ((ins & 0x3F) << 1)
        # return True
    elif op == Ops.BNE.value:
        print("BNE")
        if regfile[rs1] != regfile[rs2]:
            regfile[PC] += 2 + ((ins & 0x3F) << 1)
        # return True
    elif op == Ops.J.value:
        print("J")
        regfile[PC] = (ins & 0x3F) << 1
        # return True


    regfile[PC] += 2
    return True

def bitstring_to_bytes(s):
    return int(s, 2).to_bytes(len(s) // 8, byteorder='big')

if __name__ == "__main__":

    with open("codeinstructions", "r") as code:
        for addr, line in enumerate(code):
            c = line.split()[0].replace("_", "")
            c = bitstring_to_bytes(c)
            wordstore("code", c, addr * 2)
    with open("datamemory", "r") as code:
        for addr, line in enumerate(code):
            c = line.split()[0].replace("_", "")
            c = bitstring_to_bytes(c)
            wordstore("data", c, addr * 2)

    while instruction():
        pass
            
