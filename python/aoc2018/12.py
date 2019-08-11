import sys

def getinp():
    # inp = [+3, +3, +4, -2, -4]
    lines = sys.stdin.readlines()
    inp = []
    for l in lines:
        try:
            inp.append(int(l))
        except:
            pass

    return inp

def f(inp):
    seen = set()
    sumv = 0
    while True:
        for v in inp:
            if sumv in seen:
                return sumv
            else:
                seen.add(sumv)

            sumv += v

print f(getinp())
