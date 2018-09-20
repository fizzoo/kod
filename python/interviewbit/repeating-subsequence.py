class Solution:
    # @param A : string
    # @return an integer
    def anytwo(self, A):
        d = {x: {} for x in A}
        for i in range(len(A)):
            a = A[i]
            for j in range(i+1, len(A)):
                b = A[j]
                if b in d[a] and d[a][b] != j:
                    return 1
                
            # do separate loop to not get bug
            for j in range(i+1, len(A)):
                b = A[j]
                if b in d[a]:
                    if d[a][b] != j: 
                        # in 2 pos, so any is good in future
                        d[a][b] = -1
                else: # not seen
                    d[a][b] = j
                    
        return 0