class Solution:
    # @param A : list of integers
    # @param B : integer
    # @param C : integer
    # @return an integer
    def solve(self, A, B, C):
        C_ = [ord(a)-ord('0') for a in str(C)]
        if C >= 10**B:
            if 0 in A and B == 1:
                return len(A)
            elif 0 in A:
                return (len(A)-1) * (len(A)**(B-1))
            else:
                return len(A)**B

        if C < 10**(B-1):
            return 0

        ways = 0
        cont = False
        prev_nonzero = False
        for i, c in enumerate(C_):
            len_left = len(C_) - i -1
            cont = False
            for a in A:
                if a == 0 and not prev_nonzero:
                    pass
                elif a < c:
                    ways += len(A) ** len_left
                elif a == c and (a != 0 or prev_nonzero):
                    #okay, have 1 more way to check next time
                    cont = True
                    prev_nonzero = True
                    
            if not cont:
                break
            
        if 0 in A and B == 1:
            ways += 1

        return ways
                    
                    
                    