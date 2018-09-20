# Definition for a  binary tree node
# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None

class BSTIterator:
    # @param root, a binary search tree's root node
    def __init__(self, root):
        self.cur = root

    # @return a boolean, whether we have a next smallest number
    def hasNext(self):
        return self.cur is not None
        

    # @return an integer, the next smallest number
    def next(self):
        while True:
            if self.cur.left is None:
                res = self.cur.val
                self.cur = self.cur.right
                return res
            else:
                pre = self.cur.left
                while (pre.right is not None) and (pre.right != self.cur):
                    pre = pre.right
                if pre.right == self.cur:
                    # loop, been here already. at the top now, so go right.
                    pre.right = None
                    res = self.cur.val
                    self.cur = self.cur.right
                    return res
                else:
                    # haven't visited left tree
                    pre.right = self.cur
                    self.cur = self.cur.left

# Your BSTIterator will be called like this:
# i = BSTIterator(root)
# while i.hasNext(): print i.next(),
