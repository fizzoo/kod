class Solution:
    # @param A : list of list of integers
    # @return the same list modified
    def setZeroes(self, A):
        my = len(A)
        mx = len(A[0])
        zeroing_y = [False] * my
        zeroing_x = [False] * mx
        
        for y in range(my):
            for x in range(mx):
                if A[y][x] == 0:
                    zeroing_y[y] = True
                    zeroing_x[x] = True
                    
        for y in range(my):
            for x in range(mx):
                if zeroing_y[y] or zeroing_x[x]:
                    A[y][x] = 0
                    
        return A