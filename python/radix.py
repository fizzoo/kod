import numpy as np
import numpy.random as npr

def radixsort(lst):
    """Integer only"""
    finished = False
    low_order = 1
    while not finished:
        buckets = [[] for _ in range(10)]

        for elem in lst:
            digit = (elem // low_order) % 10
            buckets[digit].append(elem)

        j = 0
        finished = True
        for i in range(10):
            n = len(buckets[i])
            if i and n:
                finished = False

            lst[j:j+n] = buckets[i]
            j = j+n

        low_order *= 10

    return lst

def is_sorted(l):
    return all(a <= b for a, b in zip(l, l[1:]))

if __name__ == '__main__':
    for i in range(100):
        lst = npr.randint(0, 10000, 16)
        radixsort(lst)
        assert is_sorted(lst)
        print(lst)
