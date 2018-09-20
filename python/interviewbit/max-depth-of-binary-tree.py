# Definition for a  binary tree node
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution:
    def maxdepth(self, A):
        if A is None:
            return 0
        
        return 1 + max(self.maxdepth(A.left), self.maxdepth(A.right))
        
    # @param A : root node of tree
    # @return an integer
    def maxDepth(self, A):
        return self.maxdepth(A)