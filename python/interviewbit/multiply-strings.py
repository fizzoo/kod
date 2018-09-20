class Solution:
    def add_safe(self, arr, i, v):
        # add v to index i of arr.
        # expand arr if necessary.
        if i >= len(arr):
            arr[len(arr):i+1] = [0 for _ in range(len(arr), i+1)]
        
        arr[i] += v
    
    # @param A : string
    # @param B : string
    # @return a strings
    def multiply(self, A, B):
        mem = []
        Ai, Bi = A[::-1], B[::-1]
        
        for i in range(len(A)):
            for j in range(len(B)):
                self.add_safe(mem, i+j, int(Ai[i])*int(Bi[j]))
                
        for i in range(len(mem)):
            self.add_safe(mem, i+1, int(mem[i]/10))
            mem[i] = mem[i] % 10
            
        return ''.join([str(x) for x in mem[::-1]])
            
            