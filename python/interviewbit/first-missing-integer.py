class Solution:
    # @param A : list of integers
    # @return an integer
    def firstMissingPositive(self, A):
        maxd = len(A) + 5
        for i in range(len(A)):
            if A[i] < 0 or A[i] > len(A):
                A[i] = 0
                
        A.append(0)
        for a in A:
            A[a % maxd] += maxd

        for i in range(1, len(A)):
            if A[i] < maxd:
                return i
        return len(A)