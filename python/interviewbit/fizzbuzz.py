class Solution:
    def fb(self, A):
        d3 = A % 3 == 0
        d5 = A % 5 == 0
        if d3 and d5:
            return "FizzBuzz"
        elif d3:
            return "Fizz"
        elif d5:
            return "Buzz"
        else:
            return str(A)
            
    # @param A : integer
    # @return a list of strings
    def fizzBuzz(self, A):
        return [self.fb(i) for i in range(1, A+1)]