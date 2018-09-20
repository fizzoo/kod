class Solution:
    # @param A : list of strings
    # @return a strings
    def longestCommonPrefix(self, A):
        n_strs = len(A)
        res = ""
        idx = 0
        while True:
            if idx >= len(A[0]):
                return res
            cur = A[0][idx]
            for i in range(1, n_strs):
                if idx > len(A[i]) or A[i][idx] != cur:
                    return res
                    
            idx += 1
            res += cur