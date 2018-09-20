from heapq import *
class Solution:
    # @param A : integer
    # @param B : list of list of integers
    # @return an integer
    def solve(self, A, B):
        conns = [[] for _ in range(A)]
        Q = [(0 if i == 0 else 99999999, i) for i in range(A)]
        heapify(Q)
        C = [0 if i == 0 else 99999999 for i in range(A)]
        F = [False for _ in range(A)]
 
        for n1, n2, w in B:
            conns[n1-1].append((w, n2-1))
            conns[n2-1].append((w, n1-1))
            
        while Q:
            w, i = heappop(Q)
            if F[i] or w > C[i]:
                # have a better one coming up, or done already
                continue
 
            F[i] = True
            for o_w, o_i in conns[i]:
                if not F[o_i] and o_w < C[o_i]:
                    C[o_i] = o_w
                    heappush(Q, (o_w, o_i))
        
        
        return sum(C)