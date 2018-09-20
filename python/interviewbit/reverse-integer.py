class Solution:
    # @param A : integer
    # @return an integer
    def reverse(self, A):
        m = []
        sgn_pos = A >= 0
        if not sgn_pos:
            A = -A
            
        while A > 0:
            m.append(A % 10)
            A /= 10
        
        res = 0
        mult = 1
        for a in m[::-1]:
            res += a*mult
            mult *= 10
            
        if res >> 31:
            res = 0
            
        if not sgn_pos:
            res = -res

        
        return res
            
            
            