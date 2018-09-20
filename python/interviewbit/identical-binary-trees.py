
class Solution:
    def same(self, a, b):
        if a is None and b is None:
            return True
        if (a is None) != (b is None) or (a.val != b.val):
            return False
        
        return self.same(a.left, b.left) and self.same(a.right, b.right)
        
    # @param A : root node of tree
    # @return an integer
    def isSameTree(self, A, B):
        return 1 if self.same(A, B) else 0
