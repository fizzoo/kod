# Definition for a undirected graph node
# class UndirectedGraphNode:
#     def __init__(self, x):
#         self.label = x
#         self.neighbors = []

class Solution:
    # @param node, a undirected graph node
    # @return a undirected graph node
    def cloneGraph(self, node):
        root = UndirectedGraphNode(node.label)
        node.clone = root
        stack = [(root, x) for x in node.neighbors]
        while stack:
            connectee, node = stack.pop()
            if hasattr(node, 'clone'):
                connectee.neighbors.append(node.clone)
            else:
                node.clone = UndirectedGraphNode(node.label)
                connectee.neighbors.append(node.clone)
                stack.extend([(node.clone, x) for x in node.neighbors])
                
        return root
