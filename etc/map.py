
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
