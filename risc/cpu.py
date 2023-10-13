#mini implemetation of a 16 bit risc cpu, with 32 16 bit registers
#hardcoded to use 0x20 bytes of code memory and 0x20 bytes of data memory
#harvard architecture
#code memory is loaded from codeinstructions
#data memory is loaded from datamemory

from enum import Enum
import os

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
    for i in range(32):
        if i != 0 and i % 8 == 0:
            pp += "\n"
        pp += " %3s: %04x" % ("x%d" % i, regfile[i])
    pp += "\n PC: %08x" % regfile[PC]
    print('Registers')
    print(''.join(pp))
    print('data Memory')
    pp = []
    for i in range(0, len(dataMemory), 2):
        if i != 0 and i % 8 == 0:
            pp += "\n"
        pp += " %3s: %04x" % ("x%d" % i, int.from_bytes(dataMemory[i: i + 2], byteorder= 'big'))
    print(''.join(pp))

def rs(addr):
    ret = codeMemory[addr:addr+2]
    return int.from_bytes(ret, byteorder='big')

def instruction():
    ins = rs(regfile[PC])
    if ins == 0:
        print("halt")
        return False
    op  = ins >> 12
    rs1 = ins >> 9 & 0x07
    rs2 = ins >> 6 & 0x07
    ws  = ins >> 3 & 0x07
    offset = ins & 0x3F
    if op == Ops.LW.value:
        regfile[ws] = dataMemory[regfile[rs1] + offset: regfile[rs1] + offset + 2]
        regfile[ws] = int.from_bytes(regfile[ws], byteorder='big')
    elif op == Ops.SW.value:
        dest = regfile[rs1] + offset
        wordstore("data", regfile[rs2], dest)
    elif op == Ops.ADD.value:
        regfile[ws] = regfile[rs1] + regfile[rs2]
    elif op == Ops.SUB.value:
        regfile[ws] = regfile[rs1] - regfile[rs2]
    elif op == Ops.INV.value:
        regfile[ws] = ~regfile[rs1]
    elif op == Ops.LSL.value:
        regfile[ws] = regfile[rs1] << regfile[rs2]
    elif op == Ops.LSR.value:
        regfile[ws] = regfile[rs1] >> regfile[rs2]
    elif op == Ops.AND.value:
        regfile[ws] = regfile[rs1] & regfile[rs2]
    elif op == Ops.OR.value:
        regfile[ws] = regfile[rs1] | regfile[rs2]
    elif op == Ops.SLT.value:
        regfile[ws] = 1 if regfile[rs1] < regfile[rs2] else 0
    elif op == Ops.BEQ.value:
        if regfile[rs1] == regfile[rs2]:
            regfile[PC] += 2 + ((ins & 0x3F) << 1)
    elif op == Ops.BNE.value:
        if regfile[rs1] != regfile[rs2]:
            regfile[PC] += 2 + ((ins & 0x3F) << 1)
    elif op == Ops.J.value:
        regfile[PC] = (ins & 0x0C) << 1
    else:
        raise Exception("Unknown instruction")
    regfile[PC] += 2
    return True

def bitstring_to_bytes(s):
    return int(s, 2).to_bytes(len(s) // 8, byteorder='big')

if __name__ == "__main__":

    if os.path.isfile("codeinstructions") == False:
        raise Exception("codeinstructions not found")
    if os.path.isfile("datamemory") == False:
        raise Exception("datamemory not found")

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

    dump()   
    print("Running")
    while instruction():
        pass
    dump()   
