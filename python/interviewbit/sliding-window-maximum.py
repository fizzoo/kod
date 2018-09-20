from collections import deque

class Solution:
    # @param A : tuple of integers
    # @param B : integer
    # @return a list of integers
    def slidingMaximum(self, A, B):
        w = B
        if w >= len(A):
            return [max(A)]
        if w == 1:
            return A
            
        dq = deque()
        res = []
        
        dq.append((0, A[0]))

        # load dq while not outputting anything
        for i in range(1, w-1):
            # Don't keep elements smaller than current if also lower index.
            while dq and A[i] > dq[-1][1]:
                dq.pop()
            
            dq.append((i, A[i]))

        for i in range(w-1, len(A)):
            if dq[0][0] <= i-w:
                dq.popleft()
                
            while dq and A[i] > dq[-1][1]:
                dq.pop()
            
            dq.append((i, A[i]))
            res.append(dq[0][1])

        return res
            