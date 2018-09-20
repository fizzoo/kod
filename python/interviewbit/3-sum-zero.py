class Solution:
    # @param A : list of integers
    # @return a list of list of integers
    def threeSum(self, A):
        A = sorted(A)
        d = {}
        for a in A:
            if a in d:
                d[a] += 1
            else:
                d[a] = 1
                
        res = set()
        for i in range(len(A)):
            for j in range(i+1, len(A)):
                a = A[i]
                b = A[j]
                c = -(a+b)
                if c >= b and c in d:
                    if a == 0 and b == 0:
                        if d[0] >= 3:
                            res.add((a, b, c))
                    elif b == c:
                        if d[b] >= 2:
                            res.add((a, b, c))
                    else:
                        res.add((a, b, c))
                
        return list(res)
                
                
                
                