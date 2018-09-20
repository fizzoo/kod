class Solution:
    # @param A : tuple of integers
    # @return an integer
    def lis(self, A):
        return max(self.increasing(A))

    def increasing(self, A):
        lengths = [1] * len(A)
        for i in range(1, len(A)):
            for j in range(i):
                if A[j] < A[i]:
                    lengths[i] = max(lengths[i], lengths[j]+1)
                    
        return lengths