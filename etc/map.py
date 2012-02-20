def first(seq): return seq[0]
def rest(seq): return seq[1:]

assert 1 == first([1, 2, 3])
assert [2, 3] == rest([1, 2, 3])


def map2(fn, seq):
    return [fn(item) for item in seq]

assert [2,4,6] == map2(lambda x: 2*x, [1,2,3])


def map3(fn, seq, mapped=[]):
    if len(seq) == 0: return mapped
    else: return map3(fn, seq[1:], mapped + [fn(seq[0])])

assert [2,4,6] == map3(lambda x: 2*x, [1,2,3])


def cat(list, el):
    return list + [el]

assert [1,2,3] == cat([1,2], 3)

def map4(fn, seq):
    return reduce(
        lambda list, el: cat(list, fn(el)),
        seq, [])

assert [2,4,6] == map4(lambda x: 2*x, [1,2,3])
