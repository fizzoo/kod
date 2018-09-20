class Solution:
    # @param A : integer
    # @param B : integer
    # @param C : integer
    # @return an integer
    def pow(self, A, B, C):
        x = A
        n = B
        d = C
            
        if x == 0:
            return 0
        if n == 0:
            return 1
            
        res = 1

        while n > 0:
            if n & 1:
                res = (res * x) % d
                
            n = n >> 1
            x = (x * x) % d
            
        return res
            
            