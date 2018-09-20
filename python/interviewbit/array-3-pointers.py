class Solution:
    # @param A : tuple of integers
    # @param B : tuple of integers
    # @param C : tuple of integers
    # @return an integer
    def minimize(self, A, B, C):
        i = 0
        j = 0
        k = 0
        
        best = 9999999999
        while True:
            m = min([A[i], B[j], C[k]])
            best = min(best, max([abs(A[i] - B[j]), abs(B[j] - C[k]), abs(C[k] - A[i])]))
            if A[i] == m and i < len(A)-1:
                i += 1
            elif B[j] == m and j < len(B)-1:
                j += 1
            elif C[k] == m and k < len(C)-1:
                k += 1
            else:
                # Lowest one cannot be incremented, no point in continuing
                break
            
        return best