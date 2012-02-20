# n! means n * (n - 1) * ... * 3 * 2 * 1
# For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800
# Let R(n) equal the sum of the digits in the number n!
# For example, R(10) is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# Find the lowest value for n where R(n) is 8001.

from operator import mul, add

#sys.path.append('') #for emacs
from lazy import ftake, ffilter

oneTo = lambda n: range(1, n+1)
assert [1,2,3,4,5] == oneTo(5)

def count_seq():
    "partial implentation of itertools.count"
    def _():
        _.i += 1
        return _.i
    _.i = 0
    return _

assert [1,2,3,4,5] == ftake(count_seq(), 5)


def fac_seq():
    def _():
        _.count += 1
        _.acc *= _.count
        return _.acc
    _.acc = 1
    _.count = 0
    return _

assert [1, 2, 6, 24, 120] == ftake(fac_seq(), 5)

def reduce_seq(f, fnext, acc):
    def _():
        _.acc = f(_.acc, fnext())
        return _.acc
    _.acc = acc
    return _

def fac_seq2():
    def _():
        _.acc *= _.count()
        return _.acc
    _.acc = 1
    _.count = count_seq()
    return _

fac_seq3 = lambda: reduce_seq(mul, count_seq(), 1)
assert [1,2,6,24,120] == ftake(fac_seq3(), 5)

#fac2 = lambda N: reduce(mul, ftake(count_seq(), N))
#assert 120 == fac2(5)

#fac = lambda n: reduce(mul, oneTo(n), 1)
#assert 3628800 == fac(10)

digits = lambda n: map(int, str(n))
assert [1, 2, 3] == digits(123)

def digits_seq(fnext):
    def _():
        return digits(fnext())
    return _

assert [[1], [2], [6], [2, 4], [1, 2, 0]] ==  ftake(digits_seq(fac_seq3()), 5)


digits_seq2 = lambda fnext: lambda: digits(fnext())
assert [[1], [2], [6], [2, 4], [1, 2, 0]] ==  ftake(digits_seq2(fac_seq3()), 5)


def R_seq():
    def _():
        # this math should be lazy, its overusing memory
        return sum(digits(_.fac())), _.count()
    _.fac = fac_seq3()
    _.count = count_seq()
    return _

assert [(27, 9), (27, 10)] == ftake(R_seq(), 10)[8:]
assert [(8001, 787)] == ftake(ffilter(lambda n: n[0] == 8001, R_seq()), 1)

print ftake(ffilter(lambda n: n[0] == 8001, R_seq()), 1)[0][1]

#R = lambda n: sum(digits(fac(n)))
#assert 27 == R(10)

#r = range(1,800)
#print filter(lambda n: n[0]==8001, zip(map(R, r), r))

