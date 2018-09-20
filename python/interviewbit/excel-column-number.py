class Solution:
    # @param A : string
    # @return an integer
    def titleToNumber(self, A):
        res = 0
        for a in A:
            res *= 26
            res += ord(a)-ord('A')+1
            
        return res