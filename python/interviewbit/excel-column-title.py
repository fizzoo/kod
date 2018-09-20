class Solution:
    # @param A : integer
    # @return a strings
    def convertToTitle(self, A):
        asciistart = ord('A')
        
        res = ""
        while A > 0:
            a = A % 26
            if a == 0:
                a = 26
                A -= 26
            
            A /= 26

                
            res += chr(asciistart+a-1)
            
        return res[::-1]