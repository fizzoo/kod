class Solution:
    # @param A : list of integers
    # @return an integer
    def solve(self, A):
        A.sort()
         
        c = A[-1]
        cm = 0
        m = 0
        greaters = [0]*len(A)
        for i in range(len(A)-1, -1, -1):
            if A[i] == c:
                cm += 1
            else:
                m += cm
                cm = 1
                c = A[i]
                
            greaters[i] = m

                
        for a, b in zip(A, greaters):
            if a == b:
                return 1
                
        return -1