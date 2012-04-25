from operator import add

def first(seq): return seq[0]
def rest(seq): return seq[1:]

assert 1 == first([1, 2, 3])
assert [2, 3] == rest([1, 2, 3])


def map2(fn, seq):
    "imperative map"
    return [fn(item) for item in seq]

assert [2,4,6] == map2(lambda x: 2*x, [1,2,3])


def reduce2(fn, seq, acc):
    if seq == []: return acc
    else: return reduce2(fn, rest(seq), fn(acc, first(seq)))

assert 6 == reduce2(add, [1,2,3], 0)


def cat(list, el):
    return list + [el]

assert [1,2,3] == cat([1,2], 3)

def map3(fn, seq):
    accum = lambda list, el: cat(list, fn(el))
    return reduce2(accum, seq, [])

assert [2,4,6] == map3(lambda x: 2*x, [1,2,3])



def imap(f, gen):
    "implementation of itertools.imap"
    while True: yield f(gen.next())

#def izip(*seqs):
#    "implementation of itertools.izip"
#    return imap(lambda *items: items, *seqs)

#assert [(1, 'a'), (2, 'b'), (3, 'c')] == izip([1,2,3],['a','b','c'])

def count(start=0):
    "impl of itertools.count"
    while True:
        yield start
        start += 1

def take(gen, N):
    "equivalent to list(islice(gen, N))"
    fnext = lambda: gen.next()
    return [fnext() for _ in xrange(N)]

#from itertools import izip
#with open('/etc/passwd', 'r') as f:
#    zipped = izip(count(), f.readlines())
#    print take(zipped, 10)
#    for numbered in zipped: print numbered


def head(seq): return seq[0]
def rest(seq): return seq[1:]
def transpose(mtx):
    a = map(head, mtx)
    b = map(rest, mtx)
    return [a] + ([transpose(b)] if b[0] else [])


def zip(xs, ys):
    return transpose((xs, ys))

print zip([1,2,3],["a","b","c"])


def zip2(*seqs):
    return transpose(seqs)

print zip2([1,2,3],["a","b","c"])
