class Solution:
    # @param A : list of list of integers
    # @return the same list modified
    def rotate(self, A):
        n = len(A)
        if len(A) <= 1:
            return A
            
        for i in range(n//2):
            for j in range(i, n-1-i):
                tli = i
                tlj = j
                tri = j
                trj = n-1-i
                bri = n-1-i
                brj = n-1-j
                bli = n-1-j
                blj = i
                
                A[tri][trj], A[bri][brj], A[bli][blj], A[tli][tlj] = A[tli][tlj], A[tri][trj], A[bri][brj], A[bli][blj]
                
        return A