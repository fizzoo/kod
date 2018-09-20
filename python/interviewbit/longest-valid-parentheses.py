def accumulate(L):
    s = 0
    for l in L:
        s += l
        yield s
    
class Solution:
    # @param A : string
    # @return an integer
    def longestValidParentheses(self, A):
        cs = list(accumulate([1 if x == '(' else -1 for x in A]))
        
        # d is: (number on a spot: its index)
        # only store the lowest idx for a number that is not broken by lower-value
        m = 0
        d = {}
        for i in range(len(A)):
            if A[i] == '(':
                # can start here
                if cs[i] not in d:
                    d[cs[i]] = i
            else:
                # ')', can either end or be lower, breaking something
                # since we are at height c now, height c+1 will not work
                if cs[i]+2 in d:
                    del d[cs[i] +2]
                if cs[i]+1 in d:
                    m = max(m, i - d[cs[i]+1]+1)
            
        return m
                
                
                