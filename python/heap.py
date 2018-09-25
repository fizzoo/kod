class Heap():
    # max heap
    def __init__(self, enum=None):
        self.arr = []

        if enum is not None:
            for e in enum:
                self.add(e)

    def __nonzero__(self):
        return self.arr.__nonzero__()

    def __iter__(self):
        while self:
            yield self.pop()

    def __repr__(self):
        return "arr: " + str(self.arr) + " , ok: " + str(self._ok_heap())

    def _ok_heap(self, i=0):
        pi1, pi2 = self._children(i)
        la = len(self.arr)

        if pi1 < la and self.arr[pi1] > self.arr[i]:
            return False
        if pi2 < la and self.arr[pi2] > self.arr[i]:
            return False

        if pi2 < la:
            return self._ok_heap(pi1) and self._ok_heap(pi2)
        else:
            return True


    def add(self, e):
        i = len(self.arr)
        self.arr.append(e)
        self.perc_up(i)

    def perc_up(self, i):
        while i > 0:
            pi = self._parent(i)
            if self.arr[i] > self.arr[pi]:
                self.arr[i], self.arr[pi] = self.arr[pi], self.arr[i]
                i = pi
            else:
                break

    def perc_down(self, i):
        while True:
            left = True

            pi1, pi2 = self._children(i)
            if pi1 >= len(self.arr):
                return
            elif pi2 >= len(self.arr):
                # empty on right
                left = True
            elif self.arr[pi1] >= self.arr[pi2]:
                left = True

            if left and self.arr[pi1] > self.arr[i]:
                self.arr[pi1], self.arr[i] = self.arr[i], self.arr[pi1]
                i = pi1
            elif self.arr[pi2] > self.arr[i]:
                self.arr[pi2], self.arr[i] = self.arr[i], self.arr[pi2]
                i = pi2


    def peek(self):
        return self.arr[0]

    def pop(self):
        res = self.arr[0]
        self.arr[0] = self.arr[-1]
        del self.arr[-1]
        self.perc_down(0)

        return res

    def _parent(self, i):
        return (i-1) // 2

    def _children(self, i):
        a = (i+1) * 2
        return a-1, a


h = Heap([1, 2, 3, 2, 1, 0, -1, -2, 5, 7, 8])
print(list(h))
