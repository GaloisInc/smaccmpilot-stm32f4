import struct

fbo = 0x7e
ceo = 0x7c

def escape(b):
    return b ^ 0x20

def frame(buf, tag):
    res  = struct.pack('<B', fbo)
    res += struct.pack('<B', tag)
    for i in range(len(buf)):
        b = struct.unpack_from('<B', buf, i)[0]
        if b == fbo or b == ceo:
            res += struct.pack('<B',ceo)
            res += struct.pack('<B',escape(b))
        else:
            res += struct.pack('<B',b)
    res += struct.pack('<B', fbo)
    return res

def untag(buf):
    if len(buf) > 0:
        t = struct.unpack_from('<B', buf, 0)[0]
        if len(buf) > 1:
            s = struct.pack('<')
            for i in range(1,len(buf)):
                b = struct.unpack_from('<B', buf, i)[0]
                s += struct.pack('<B', b)
            return (t,s)
        else:
            return (t,None)
    return (None, None)

class Unframer(object):
    def __init__(self):
        self.begin = False
        self.escape = False
        self.frame = []

    def parse(self, buf):
        b = struct.unpack_from('<B',buf)[0]
        if self.begin:
            if b == fbo:
                if len(self.frame) > 0:
                    return True
                else:
                    return False
            elif b == ceo:
                self.escape = True
            else:
                self.frame.append(self.escaped(b))
        elif b == fbo:
            self.begin = True
        return False

    def escaped(self, b):
        if self.escape:
            self.escape = False
            return escape(b)
        else:
            return b

    def getframe(self):
        f = struct.pack('<')
        for i in range(len(self.frame)):
            f += struct.pack('<B', self.frame[i])
        self.escape = False
        self.frame = []
        return f
