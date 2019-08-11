import sys, collections

def similar(x, y):
    edit_dist = 0
    for a, b in zip(x, y):
        if a != b:
            edit_dist += 1
            if edit_dist > 1:
                return False

    return True

def remove_dissimilar(x, y):
    res = []
    for a, b in zip(x, y):
        if a == b:
            res.append(a)

    return "".join(res)

def f():
    lines = [x[:-1] for x in sys.stdin.readlines()]

    for i in range(len(lines)):
        for j in range(i):
            a, b = lines[i], lines[j]
            if similar(a, b):
                print(remove_dissimilar(a, b))
                return

print(f())
