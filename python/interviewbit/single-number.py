class Solution:
    # @param A : tuple of integers
    # @return an integer
    def singleNumber(self, A):
        x = 0
        for a in A:
            x = x ^ a
            
        return x