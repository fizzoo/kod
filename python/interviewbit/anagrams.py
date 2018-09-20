class Solution:
    # @param A : tuple of strings
    # @return a list of list of integers
    def anagrams(self, A):
        d = {}
        
        for i, a in enumerate(A):
            a = str(sorted(a))
            if a in d:
                d[a].append(i+1)
            else:
                d[a] = [i+1]
                
        return list(d.values())