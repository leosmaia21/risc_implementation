PC = 0
# memory = b'\x00'*0x10000
memory = b'\x00'*0x100

def ws(data, addr):
    global memory
    memory = memory[:addr] + data + memory[addr+len(data):]

if __name__ == "__main__":

    with open("codeinstructions", "r") as code:
        for addr, line in enumerate(code):
            c = line[:19]
            c = (int)(c.replace("_", ""))
            ws(c.to_bytes(2), addr)

    print(memory)
            















