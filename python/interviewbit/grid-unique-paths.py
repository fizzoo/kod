class Solution:
    def step(self, mat, y, x):
        steps = mat[y][x]
        if steps >= 0:
            return steps
        
        steps = self.step(mat, y+1, x) + self.step(mat, y, x+1)
        mat[y][x] = steps
        return steps
        
    # @param A : integer
    # @param B : integer
    # @return an integer
    def uniquePaths(self, A, B):
        my = A
        mx = B
        mat = [[-1]*mx for _ in range(my)]
        
        mat[-1][:] = [1]*mx
        for y in range(my):
            mat[y][-1] = 1
            
        return self.step(mat, 0, 0)