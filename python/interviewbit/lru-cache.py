class denode():
    def __init__(self, fr, to, p=None, n=None):
        self.fr = fr
        self.to = to
        self.p = p
        self.n = n
        
    def __repr__(self):
        return str(self.fr) + ": " + str(self.to)

class LRUCache:

    # @param capacity, an integer
    def __init__(self, capacity):
        self.cap = capacity
        self.mushmup = {}
        p = denode(1000, -1)
        self.mushmup[1000] = p
        self.ll_start = p
        
        for i in range(1, capacity):
            n = denode(1000+i, -1, p)
            self.mushmup[1000+i] = n
            p.n = n
            p = n
        self.ll_end = p
        

    def push_up(self, n):
        if self.cap == 1:
            # do nothing
            pass
        elif n == self.ll_start:
            # from first, no n to edit
            # fix start
            self.ll_start = n.n
            self.ll_start.p = None
            
            # fix end
            prev_last = self.ll_end
            prev_last.n = n
            self.ll_end = n
            n.n = None
            n.p = prev_last
        elif n == self.ll_end:
            # do nothing, already at end
            pass
        else:
            # patch hole
            prev_prev = n.p
            prev_next = n.n
            prev_prev.n = prev_next
            prev_next.p = prev_prev
            
            # fix end
            prev_last = self.ll_end
            prev_last.n = n
            self.ll_end = n
            n.n = None
            n.p = prev_last

    def _corrnr_nodes(self):
        nr = 0
        n = self.ll_start
        while n is not None:
            nr += 1
            n = n.n
            
        assert nr == self.cap
        
    def _corrnr_mush(self):
        assert len(self.mushmup) == self.cap

    # @return an integer
    def get(self, key):
        if key in self.mushmup:
            n = self.mushmup[key]
            self.push_up(n)
            return n.to
        else:
            return -1
            
            

    # @param key, an integer
    # @param value, an integer
    # @return nothing
    def set(self, key, value):
        if key in self.mushmup:
            n = self.mushmup[key]
            n.to = value
            self.push_up(n)
        else:
            # can reuse last value and just push it up
            n = self.ll_start
            
            # fix hashmap
            del self.mushmup[n.fr]
            self.mushmup[key] = n
            
            # reuse node, push it up
            n.fr = key
            n.to = value
            self.push_up(n)




