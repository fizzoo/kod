class Solution:
    # @param A : list of integers
    # @return an integer
    def maxSpecialProduct(self, A):
        if len(A) <= 2:
            return 0
            
        res = 0
        # "stacks" of v, i, for value, index.
        lsta = []
        rsta = []

        lsps = [0] * len(A)
        rsps = [0] * len(A)
        
        for i in range(len(A)):
            ai = A[i]
            # keep only things higher than this, cause if they're lower this one has a better index.
            while lsta and lsta[-1][0] <= ai:
                lsta.pop()
            if lsta:
                # any previous value higher than this? take the latest one.
                lsps[i] = lsta[-1][1]
                
            lsta.append((ai, i))

        for i in range(len(A)-1, -1, -1):
            ai = A[i]
            # keep only things higher than this, cause if they're lower this one has a better index.
            while rsta and rsta[-1][0] <= ai:
                rsta.pop()
            if rsta:
                # any previous value higher than this? take the latest one.
                rsps[i] = rsta[-1][1]
                
            rsta.append((ai, i))

        for i in range(1, len(A)-1):
            res = max(res, lsps[i] * rsps[i])
            
        #print(lsps, rsps)
            
        return res % 1000000007