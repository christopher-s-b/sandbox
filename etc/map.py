def first(seq): return seq[0]
def rest(seq): return seq[1:]

assert 1 == first([1, 2, 3])
assert [2, 3] == rest([1, 2, 3])


def map2(fn, seq):
    r = []
    for item in seq:
        r.append(fn(item))
    return r

assert [2,4,6] == map2(lambda x: 2*x, [1,2,3])


def map3(fn, seq, mapped=[]):
    if len(seq) == 0: return mapped
    else: return map3(fn, seq[1:], mapped + [fn(seq[0])])


assert [2,4,6] == map3(lambda x: 2*x, [1,2,3])
