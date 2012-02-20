from itertools import islice, imap, ifilter

def ftake(fnext, last):
    return [fnext() for _ in xrange(last)]

def gtake(gen, N):
    "equivalent to list(islice(gen, N))"
    return ftake(lambda: gen.next(), N)

def fib_gen():
    a, b = 1, 1
    while True:
        yield a
        a, b = b, a + b

assert [1, 1, 2, 3, 5] == list(islice(fib_gen(), 5))
assert [1, 1, 2, 3, 5] == gtake(fib_gen(), 5)

def fib_gen2():
    #funky scope due to python2.x workaround
    #for python 3.x use nonlocal
    def _():
        _.a, _.b = _.b, _.a + _.b
        return _.a
    _.a, _.b = 0, 1
    return _
assert [1,1,2,3,5] == ftake(fib_gen2(), 5)


class fib_gen3:
    def __init__(self):
        self.a, self.b = 1, 1

    def __call__(self):
        r = self.a
        self.a, self.b = self.b, self.a + self.b
        return r

assert [1,1,2,3,5] == ftake(fib_gen3(), 5)

def ffilter(pred, fnext):
    def next():
        x = fnext()
        if pred(x): return x
        else: return next()
    return next

assert [13, 21, 34] == ftake(ffilter(lambda x: x > 10, fib_gen2()), 3)
assert [13, 21, 34] == ftake(ffilter(lambda x: x > 10, fib_gen3()), 3)

def gfilter(pred, gen):
    "implementation of itertools.ifilter"
    while True:
        x = gen.next()
        if pred(x):
            yield x

assert [13, 21, 34] == gtake(gfilter(lambda x: x > 10, fib_gen()), 3)
assert [13, 21, 34] == list(islice(ifilter(lambda x: x > 10, fib_gen()), 3))


def fmap(f, fnext):
    return lambda: f(fnext())

assert [2,2,3,4,6] == ftake(fmap(lambda x: x+1, fib_gen2()), 5)
assert [2,2,3,4,6] == ftake(fmap(lambda x: x+1, fib_gen3()), 5)

def gmap(f, gen):
    "implementation of itertools.imap"
    while True: yield f(gen.next())

assert [2,2,3,4,6] == gtake(gmap(lambda x: x+1, fib_gen()), 5)
assert [2,2,3,4,6] == list(islice(imap(lambda x: x+1, fib_gen()), 5))
