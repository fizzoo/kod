# discussion: Hashmap here makes it more general, can deal with arbitrary strings, but there is some constant overhead to them. Could do fixed-size lists since we only have 10 distinct possibilities, but it would require notably more code.

# The trie can only handle string-like types, so we may freely automatically convert to string for ease of use.
# add() and contains() requires a string so that they're not misused too hard, but the constructor and *in* operator does not for ease of use.

class Trie():
    """Trie without values, since we only care about prefixes we do not have to record where the strings end."""
    def __init__(self, lst=[]):
        """Initialize from enumerable 'lst' representing the set of strings.

        O(S) where S is the sum of the lengths of all strings in set 'lst'."""
        self.root = {}

        for element in lst:
            self.add(str(element))

    def __contains__(self, elem):
        """Check whether elem is a prefix of any string in the set.

        O(M), where M is the length of string 'elem'."""
        return self.contains(str(elem))

    def add(self, string):
        """Add string 'string' to the set of strings.

        O(M), where M is the length of 'string'."""
        cur = self.root
        for s in string:
            if s in cur:
                cur = cur[s]
            else:
                cur[s] = {}
                cur = cur[s]

    def contains(self, string):
        """Check whether string 'string' is a prefix of any string in the set.

        O(M), where M is the length of 'string'."""
        cur = self.root
        for s in string:
            if s in cur:
                cur = cur[s]
            else:
                return False

        return True

def main():
    lst = [110, 112, 911, 110991, 991110, 123456789, 873625]
    t = Trie(lst)

    assert 110 in t
    assert 11 in t
    assert 9911 in t
    assert 123 in t
    assert 123456789 in t
    assert 873 in t
    assert '' in t
    assert 1 in t
    assert 11111 not in t
    assert 5 not in t

    # Stricter interface + other way to initialize
    t2 = Trie()
    t2.add(str(123))
    t2.add(str(321))
    assert t2.contains(str(1))
    assert not t2.contains(str(2))

if __name__ == '__main__':
    main()
