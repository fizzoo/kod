class Solution:
    def gen(self, lefties, righties):
        if righties == 0:
            return [""]
        
        res = []
        if lefties > 0:
            res += ["(" + x for x in self.gen(lefties-1, righties)]
        
        if righties > lefties:
            # Can choose to close here
            res += [")" + x for x in self.gen(lefties, righties-1)]
            
        return res
        
    # @param A : integer
    # @return a list of strings
    def generateParenthesis(self, A):
        return self.gen(A, A)