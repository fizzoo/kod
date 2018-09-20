class Solution:
    def concat(self, lst):
        return [x for y in lst for x in y]
        
    def gen(self, A):
        if len(A) <= 1:
            return [A]
            
        res = []
        for i in range(len(A)):
            res += [A[i:i+1] + x for x in self.gen(A[:i] + A[i+1:])]
            
        return res
        
    # @param A : list of integers
    # @return a list of list of integers
    def permute(self, A):
        return self.gen(A)