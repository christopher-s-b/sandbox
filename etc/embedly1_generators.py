# n! means n * (n - 1) * ... * 3 * 2 * 1
# For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800
# Let R(n) equal the sum of the digits in the number n!
# For example, R(10) is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# Find the lowest value for n where R(n) is 8001.

from operator import mul, add
from itertools import ifilter, islice, count

def fac_gen():
    acc, count = 1, 0
    while True:
        count += 1
        acc *= count
        yield acc

assert [1,2,6,24,120] == list(islice(fac_gen(), 5))

digits = lambda n: map(int, str(n))
assert [1, 2, 3] == digits(123)

def R_seq():
    gfac = fac_gen()
    gcount = count()
    while True:
        yield sum(digits(gfac.next())), gcount.next()+1

assert [(27, 9), (27, 10)] == list(islice(R_seq(), 8, 10))
assert [(8001, 787)] == list(islice(ifilter(lambda n: n[0] == 8001, R_seq()), 1))

def embedly1():
    print list(islice(ifilter(lambda n: n[0] == 8001, R_seq()), 1))[0][1]


