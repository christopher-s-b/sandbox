class Partial:
    def __init__(self, f, *args):
        self.args = args
        self.f = f
    def apply(self, *args):
        return self.f(*(self.args + args))

add = lambda x,y: x+y
assert Partial(add, 5).apply(4) == 9
