class Solution:
    # @param A : list of integers
    # @param B : list of integers
    # @return an integer
    def mice(self, A, B):
        mice = sorted(A)
        holes = sorted(B)
        return max([abs(a-b) for a, b in zip(mice, holes)])