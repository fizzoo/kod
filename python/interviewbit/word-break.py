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
        visited = [False] * len(A)
        
        stack.append(0)
        
        while stack:
            idx = stack.pop()
            if visited[idx]:
                continue
            visited[idx] = True
            
            if A[idx:] in word_set:
                return 1
                
            for jdx in range(idx + 1, len(A)):
                test_word = A[idx:jdx]
                if test_word in word_set:
                    stack.append(jdx)
            
        return 0
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
