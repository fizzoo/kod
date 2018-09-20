class Solution:
    def binsmaller(self, A, x, lo, hi):
        # find the first i s.t. A[i] <= x.
        # If none exists, returns None
        if lo+1 >= hi:
            if A[lo] <= x:
                return A[lo]
            else:
                return None
            
        mid = (hi + lo) // 2
        if A[mid] <= x:
            return self.binsmaller(A, x, lo, mid) or A[mid]
        else:
            return self.binsmaller(A, x, mid+1, hi)
        
    # @param A : tuple of integers
    # @return an integer
    def maximumGap(self, A):
        # stack will always be decreasing, as any value
        # higher than a previously existing one is useless
        stack = []
        maxlen = 0
        for i in range(len(A)):
            cur_p = A[i], i
            if (not stack) or (stack[-1] > cur_p):
                # smallest seen thus far, no point searching
                stack.append(cur_p)
            else:
                # something smaller exists in stack, find the leftmost one
                prev = self.binsmaller(stack, cur_p, 0, len(stack))
                if prev is not None: # might not be necessary
                    maxlen = max(maxlen, i - prev[1])
                
        return maxlen
            