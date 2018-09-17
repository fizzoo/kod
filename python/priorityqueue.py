from functools import total_ordering
from heapq import *
import numpy.random as npr

@total_ordering
class OppositeOrder:
    def __init__(self, v):
        self.v = v

    def __lt__(self, other):
        return self.v > other.v

    def __eq__(self, other):
        return self.v == other.v


if __name__ == '__main__':
    lst = [OppositeOrder(x) for x in npr.randint(0, 1000, 32)]
    heapify(lst)
    print([e.v for e in lst])
    res = []
    while lst:
        res.append(heappop(lst).v)

    print(res)
