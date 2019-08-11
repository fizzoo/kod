import sys, collections

def f():
    lines = sys.stdin.readlines()
    twos, threes = 0, 0
    for l in lines:
        s = collections.defaultdict(lambda: 0)
        for c in l:
            s[c] += 1

        v = s.values()
        if 2 in v:
            twos += 1
        if 3 in v:
            threes += 1

    return twos*threes

print(f())
