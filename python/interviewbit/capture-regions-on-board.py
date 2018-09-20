class Solution:
    # @param A : list of list of chars
    
    def fill(self, A, y, x):
        if y < 0 or x < 0 or y >= self.my or x >= self.mx:
            return
        
        if A[y][x] == 'O':
            A[y][x] = 'S'
            self.fill(A, y-1, x)
            self.fill(A, y+1, x)
            self.fill(A, y, x+1)
            self.fill(A, y, x-1)
        
    def solve(self, A):
        my = len(A)
        mx = len(A[0])
        self.mx = mx
        self.my = my
        for y in range(my):
            self.fill(A, y, 0)
            self.fill(A, y, mx-1)
        for x in range(mx):
            self.fill(A, 0, x)
            self.fill(A, my-1, x)
            
        for y in range(my):
            for x in range(mx):
                c = A[y][x]
                if c == 'O':
                    A[y][x] = 'X'
                elif c == 'S':
                    A[y][x] = 'O'
                    
        