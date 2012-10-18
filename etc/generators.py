from itertools import islice

def fib_gen():
    a, b = 1, 1
    while True:
        yield a
        a, b = b, a + b

assert [1, 1, 2, 3, 5] == list(islice(fib_gen(), 5))


def fib_gen4():
    a, b = 0, 1
    def _():
        nonlocal a, b   # lift vars to enclosing scope
        a, b = b, a+b
        return a
    return _

def ftake(fnext, N):
    "take the next N elements from a sequence by invoking it N times"
    return [fnext() for _ in xrange(N)]

assert [1,1,2,3,5] == ftake(fib_gen2(), 5)



def fib_gen2(state):
    a, b = state
    newstate = b, a+b
    return newstate

def iterate(f, x0):
    """Returns a lazy sequence of x, (f x), (f (f x)) etc.
    f must be free of side-effects"""
    xn = f(x0)


assert [1,1,2,3,5] == ftake(fib_gen2(), 5)
