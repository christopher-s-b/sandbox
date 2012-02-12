from itertools import combinations


def combos(seq):
    res = []
    for n in range(3, len(seq)+1):
        for c in combinations(seq, n):
            res.append(c)
    return res

def subsets(pred, seq):
    return filter(pred, combos(seq))

def match(combo):
    N = len(combo)
    return sum(combo[:N-1]) == combo[N-1]

assert 4 == len(subsets(match, [1,2,3,4,6]))

seq = [int(s.strip()) for s in open('greplin3.csv').read().split(",")]
print len(subsets(match, seq))
