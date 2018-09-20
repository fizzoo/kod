class Solution:
    def ones(self, A):
        return bin(A).count("1")
        
    def f(self, x, y):
        return self.ones(x ^ y)

    def binstr(self, n):
        return bin(n)[2:].zfill(32)
    
    # @param A : tuple of integers
    # @return an integer    
    def hammingDistance(self, A):
        binstrs = [self.binstr(a) for a in A]
        
        max_tot = len(binstrs)
        tots = [0] * 32
        
        for bs in binstrs:
            for i in range(32):
                tots[i] += (bs[i] == '1')
            
        sum = 0
        for i in range(32):
            t = tots[i]
            sum += t * (max_tot - t)
                
        return sum*2