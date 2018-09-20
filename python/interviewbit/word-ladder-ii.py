#2 my solution, wrote on a friends comp

from collections import deque

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
    for k, v in allbut_iterator(list(set(strings))):
        g[k] = [x for x in v if onedist(k, x)]
        
    return g

def bfsgo(start, dicter, dists):
    queue = deque([(start, [])])
    
    while queue:
        s, path = queue.pop()
        d = dists[s]
        
        if d == 0:
            # on right d, so know that d+1 is exhausted, d-1 does not exist
            queue.append((s, path))
            return [path + [s] for s, path in queue]
            
        queue.extendleft([(x, path+[s]) for x in dicter[s] if dists[x] == d-1])
            
    return []
    
def fill_dists(dicter, end):
    queue = deque([(end, 0)])
    dists = {x: 999999 for x in dicter.keys()}
    dists[end] = 0
    
    while queue:
        s, d = queue.pop()
        if dists[s] < d:
            #already visited AND had a better one that time
            continue
        
        for v in dicter[s]:
            if dists[v] > d+1:
                dists[v] = d+1
                queue.appendleft((v, d+1))
                
    return dists


class Solution:
    # @param start : string
    # @param end : string
    # @param dictV : list of strings
    # @return a list of list of strings
    def findLadders(self, start, end, dictV):
        g = buildgraph(dictV)
        dists = fill_dists(g, end)
        return bfsgo(start, g, dists)
