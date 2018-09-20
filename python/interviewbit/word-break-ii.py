class Solution:
    # @param A : string
    # @param B : list of strings
    # @return an integer
    def wordBreak(self, A, B):
        if len(A) < 1:
            return 0
            
        A = "".join(A.split())
        
        word_set = set(B)
        stack = []

        stack.append((0, []))
        res = []
        
        while stack:
            idx, prevs = stack.pop()

            if A[idx:] in word_set:
                res.append(prevs + [A[idx:]])
                
            for jdx in range(idx + 1, len(A)):
                test_word = A[idx:jdx]
                if test_word in word_set:
                    stack.append((jdx, prevs + [test_word]))
            
        return [" ".join(r) for r in sorted(res)]
            
            
            
            
            
            
            
            
        