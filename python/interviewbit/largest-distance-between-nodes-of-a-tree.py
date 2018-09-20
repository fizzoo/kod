def maxtwo(a, b, n):
    # sort s.t. a > b > n, return a, b.
    if b > a:
        # shouldn't occur if always using this one, but ensure this
        b, a = a, b
    if n > a:
        return n, a
    elif n > b:
        return a, n
    else:
        return a, b
    

class Solution:
    # @param A : list of integers
    # @return an integer
    def solve(self, A):
        self.adj = [[] for _ in A]
        for i, a in enumerate(A):
            if a == -1:
                root = i
            else:
                self.adj[a].append(i)
    
        dists = [-2] * len(A)
        maxtwos = [-2] * len(A)
        
        s = [root]
        while s:
            c = s[-1]
            children_done = True
            for child in self.adj[c]:
                if dists[child] < 0:
                    # not done yet
                    s.append(child)
                    children_done = False
                
            if children_done:
                s.pop()
                a = b = 0
                for j in self.adj[c]:
                    a, b = maxtwo(a, b, dists[j]+1)
                
                dists[c] = a
                maxtwos = a+b
                
        return max(maxtwos)
