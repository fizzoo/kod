class Solution:
    def increasing(self, A):
        lengths = [1] * len(A)
        for i in range(1, len(A)):
            for j in range(i):
                if A[j] < A[i]:
                    lengths[i] = max(lengths[i], lengths[j]+1)
                    
        return lengths
        
    # @param A : tuple of integers
    # @return an integer
    def longestSubsequenceLength(self, A):
        if len(A) <= 1:
            return len(A)
            
        inc = self.increasing(A)
        dec = self.increasing(A[::-1])[::-1]
        
        allinc = max(inc)
        alldec = max(dec)
        
        m = 0
        for i in range(len(A)-1):
            for j in range(i+1, len(A)):
                m = max(m, inc[i] + dec[j] - (A[i] == A[j]))
                
        return max([m, allinc, alldec])
            