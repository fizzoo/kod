# discussion: Hashmap here makes it more general, can deal with arbitrary strings, but there is some constant overhead to them. Could do fixed-size lists since we only have 10 distinct possibilities, but it would require notably more code.

# The trie can only handle string-like types, so we may occasionally automatically convert to string for ease of use.
# add() and is_in() requires a string so that they're not misused too hard, but the constructor and *in* operator does not for ease of use.

class Trie():
    def __init__(self, lst):
        self.root = {}

        for element in lst:
            self.add(str(element))

    def __contains__(self, elem):
        return self.is_in(str(elem))

    def add(self, string):
        cur = self.root
        for s in string:
            if s in cur:
                cur = cur[s]
            else:
                cur[s] = {}
                cur = cur[s]

    def is_in(self, string):
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

if __name__ == '__main__':
    main()
