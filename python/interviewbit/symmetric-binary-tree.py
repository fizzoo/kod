# Definition for a  binary tree node
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution:
    def sym(self, a, b):
        if a is None and b is None:
            return True
        if (a is None) != (b is None) or (a.val != b.val):
            return False
        
        return self.sym(a.left, b.right) and self.sym(a.right, b.left)
        
    # @param A : root node of tree
    # @return an integer
    def isSymmetric(self, A):
        return 1 if self.sym(A, A) else 0
