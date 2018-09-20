# Definition for a  binary tree node
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class Solution:
    
    def mindepth(self, A):
        if A is None:
            return 99999999
        if A.left is None and A.right is None:
            return 1
        
        return 1 + min(self.mindepth(A.left), self.mindepth(A.right))

    # @param A : root node of tree
    # @return an integer
    def minDepth(self, A):
        return self.mindepth(A)
