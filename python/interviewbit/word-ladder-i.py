#1 my solution, wrote on a friends comp

def onedist(a, b):
    edit = 0
    for i in range(len(a)):
        if a[i] != b[i]:
            edit += 1
            
        if edit > 1:
            return False
            
    return edit == 1
        
def allbut_iterator(a):
    for i in range(len(a)):
        yield a[i], a[:i] + a[i+1:]
        
def buildgraph(strings):
    # keys: word to go from, values: list of possible words to go to
    g = {}
    for k, v in allbut_iterator(strings):
        g[k] = [x for x in v if onedist(k, x)]
        
    return g

def bfsgo(start, end, dicter):
    queue = [(start, 0)]
    visited = {x: False for x in dicter.keys()}
    
    while queue:
        s, d = queue.pop()
        visited[s] = True
        if s == end:
            return d
            
        queue = [(x, d+1) for x in dicter[s] if not visited[x]] + queue
        
    return -1 # nothing
    
class Solution:
    # @param start : string
    # @param end : string
    # @param dictV : list of strings
    # @return an integer
    def ladderLength(self, start, end, dictV):
        return bfsgo(start, end, buildgraph(dictV)) +1


