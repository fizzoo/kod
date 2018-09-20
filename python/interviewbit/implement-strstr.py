class Solution:
    # @param A : string
    # @param B : string
    # @return an integer
    def strStr(self, A, B):
        if not A or not B:
            return -1
            
        if len(B) == 1:
            return A.find(B)
            
        # mem contains i if we've just seen B[0:i+1]
        mem = []

        for k in range(len(A)):
            a = A[k]
            j = 0
            while j < len(mem):
                i = mem[j]
                if B[i+1] == a:
                    if i+2 >= len(B):
                        return k-i-1
                    mem[j] += 1
                    j += 1
                else:
                    del mem[j]
            
            if a == B[0]:
                mem.append(0)
                
        return -1