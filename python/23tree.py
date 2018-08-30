from enum import Enum

# Self-balancing 2-3 tree

class BCase(Enum):
    Empty = 0
    Leaf1 = 1
    Leaf2 = 2
    Node2 = 3
    Node3 = 4

class BNode:
    def __init__(self, dummy=None, a=None, b=None, l=None, m=None, r=None):
        if dummy is not None:
            raise Exception("Don't put anything in, dummy.")

        self.a = a
        self.b = b
        self.l = l
        self.m = m
        self.r = r

    def case(self):
        if self.a is None:
            return BCase.Empty
        elif self.l is None and self.b is None:
            return BCase.Leaf1
        elif self.l is None and self.b is not None:
            return BCase.Leaf2
        elif self.b is None:
            return BCase.Node2
        else:
            return BCase.Node3

    def smallest(self):
        if self.l is not None:
            return self.l.smallest()
        else:
            return self.a

    def biggest(self):
        if self.r is not None:
            return self.r.biggest()
        elif self.b is not None:
            return self.b
        else:
            return self.a

    def inorder(self):
        case = self.case()
        if case == BCase.Empty:
            return []
        elif case == BCase.Leaf1:
            return [self.a]
        elif case == BCase.Leaf2:
            return [self.a, self.b]
        elif case == BCase.Node2:
            return self.l.inorder() + [self.a] + self.r.inorder()
        elif case == BCase.Node3:
            return self.l.inorder() + [self.a] + self.m.inorder() + [self.b] + self.r.inorder()


    def __repr__(self):
        case = self.case()
        if case == BCase.Empty:
            return "NULL"
        elif case == BCase.Leaf1:
            return "(" + str(self.a) + ")"
        elif case == BCase.Leaf2:
            return "(" + str(self.a) + "," + str(self.b) + ")"
        elif case == BCase.Node2:
            return "<" + str(self.a) + ":" + self.l.__repr__() + "," + self.r.__repr__() + ">"
        elif case == BCase.Node3:
            return "<" + str(self.a) + "," + str(self.b) + ":" + self.l.__repr__() + "," + self.m.__repr__() + "," + self.r.__repr__() + ">"

    def _inv(self):
        if self.r is None and self.m is not None:
            return False
        if self.l is None and self.r is not None:
            return False

        #some more possible regarding sticking to the 5 cases...

        if self.l is not None and self.l.biggest() > self.a:
            return False
        if self.b is not None and self.r is not None and self.r.smallest() < self.b:
            return False
        if self.r is not None and self.r.smallest() < self.a:
            return False
        if self.m is not None and self.m.smallest() < self.a:
            return False
        if self.m is not None and self.m.biggest() > self.b:
            return False

        l = self.inorder()
        if any([l[i] > l[i+1] for i in range(len(l)-1)]):
            return False

        return True

class BTree:
    def __init__(self):
        self.root = BNode()

    def __repr__(self):
        return self.root.__repr__()

    def insert(self, x):
        trace = [self.root]

        # go down looking for place to insert
        while True:
            c = trace[-1]
            case = c.case()
            promote = None
            split = None

            if case == BCase.Empty:
                # only on root, really.
                c.a = x
                return # nothing to fix
            elif case == BCase.Leaf1:
                # No children, make it a 2-er without nodes
                if x == c.a:
                    return

                if x < c.a:
                    c.b = c.a
                    c.a = x
                else:
                    c.b = x
                return # nothing to fix
            elif case == BCase.Leaf2:
                if x == c.a or x == c.b:
                    return

                if x < c.a:
                    l, m, r = x, c.a, c.b
                elif x < c.b:
                    l, m, r = c.a, x, c.b
                else:
                    l, m, r = c.a, c.b, x

                promote = m
                split = BNode(a=l), BNode(a=r)
                break
            elif case == BCase.Node2:
                if x == c.a or x == c.b:
                    return

                if x < c.a:
                    trace.append(c.l)
                else:
                    trace.append(c.r)
            elif case == BCase.Node3:
                if x == c.a or x == c.b:
                    return

                if x < c.a:
                    trace.append(c.l)
                elif x < c.b:
                    trace.append(c.m)
                else:
                    trace.append(c.r)

        # Now want to fix the tree by promoting, possibly rippling all the way up
        trace.pop()
        while True:
            if trace:
                c = trace.pop()
            else:
                self.root = BNode(a=promote, l=split[0], r=split[1])
                return

            case = c.case()

            if case == BCase.Node2:
                if promote < c.a:
                    c.b = c.a
                    c.a = promote
                    c.l, c.m = split
                else:
                    c.b = promote
                    c.m, c.r = split

                return # Done fixing.

            elif case == BCase.Node3:
                # need to split c too
                if promote < c.a:
                    left = BNode(a=promote, l=split[0], r=split[1])
                    right = BNode(a=c.b, l=c.m, r=c.r)
                    promote = c.a
                    split = left, right
                elif promote < c.b:
                    left = BNode(a=c.a, l=c.l, r=split[0])
                    right = BNode(a = c.b, l=split[1], r=c.r)
                    split = left, right
                else:
                    left = BNode(a=c.a, l=c.l, r=c.m)
                    right = BNode(a=promote, l=split[0], r=split[1])
                    promote = c.b
                    split = left, right

            else:
                raise Exception("weird.")
