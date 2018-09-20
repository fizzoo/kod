class Solution:
    # @param A : list of integers
    # @return a list of integers
    def wave(self, A):
        S = sorted(A)
        for i in range(0, len(A)-1, 2):
            S[i], S[i+1] = S[i+1], S[i]
            
        return S