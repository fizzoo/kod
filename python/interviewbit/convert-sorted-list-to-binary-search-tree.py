# Definition for a  binary tree node
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    
    # Array v, inclusive indexes start i and end j, make a tree.
    def makeBST(self, v, i, j):
        if i > j:
            return None
            
        mid = (i+j) // 2
        #print(v, i, j, mid)
        node = TreeNode(v[mid])
        node.left = self.makeBST(v, i, mid-1)
        node.right = self.makeBST(v, mid+1, j)
        
        return node
    
    # @param A : head node of linked list
    # @return the root node in the tree    
    def sortedListToBST(self, A):
        v = []
        c = A
        while True:
            v.append(c.val)
            if c.next is not None:
                c = c.next
            else:
                break
            
            
        return self.makeBST(v, 0, len(v)-1)
