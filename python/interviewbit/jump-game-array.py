class Solution:
    # @param A : list of integers
    # @return an integer
    def canJump(self, A):
        if len(A) <= 1:
            return 1
            
        i = 0
        maxjumpable = 0
        while i <= maxjumpable:
            maxjumpable = max(maxjumpable, A[i] + i)
            if maxjumpable >= len(A):
                return 1
            
            i += 1

        return 0
