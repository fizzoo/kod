def m(A, B):
    if not B:
        return A
    if not A:
        return B
            
    # idxs
    i, j = 0, 0
    C = []
    while i < len(A) and j < len(B):
        if A[i] > B[j]:
            C.append(B[j])
            j += 1
        else:
            C.append(A[i])
            i += 1
                
    while i < len(A):
        C.append(A[i])
        i += 1
        
    while j < len(B):
        C.append(B[j])
        j += 1
        
    A[:] = C
    
class Solution:
    # @param A : list of integers
    # @param B : list of integers
    def merge(self, A, B):
        m(A, B)
        print(" ".join(str(x) for x in A) + " ")