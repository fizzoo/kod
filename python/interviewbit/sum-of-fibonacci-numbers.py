class Solution:
    def fibupto(self, n):
        fibs = [1, 1]
        if n <= 1:
            return fibs
            
        while True:
            nf = fibs[-1] + fibs[-2]
            if nf > n:
                break
            
            fibs.append(nf)
            
        return fibs
    
    # @param A : integer
    # @return an integer
    def fibsum(self, A):
        fibs = self.fibupto(A)
        idx = len(fibs) -1
        res = 0
        while A > 0:
            nr_fits = A // fibs[idx]
            if nr_fits > 0:
               res += nr_fits
               A -= nr_fits * fibs[idx]
            
            idx -= 1
               
        return res
            
