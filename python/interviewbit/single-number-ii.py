class Solution:
    # @param A : tuple of integers
    # @return an integer
    def singleNumber(self, A):
        d = {}
        for a in A:
            if a not in d:
                d[a] = 1
            elif d[a] == 2:
                del d[a]
            else:
                d[a] = 2
                
        return d.keys()[0]