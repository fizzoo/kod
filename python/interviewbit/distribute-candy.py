class Solution:
    # @param A : list of integers
    # @return an integer
    def candy(self, A):
        order = sorted([(A[i], i) for i in range(len(A))])
        candies = [1] * len(A)
        for rank, i in order:
            if i-1 >= 0 and rank > A[i-1]:
                candies[i] = candies[i-1]+1
            if i+1 < len(A) and rank > A[i+1]:
                candies[i] = max(candies[i], candies[i+1]+1)
                
        return sum(candies)
        