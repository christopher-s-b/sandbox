# compute the smallest prime fibonacci, X, greater than 227,000
# compute sum of prime divisors of X+1

from math import sqrt

def fib_gen():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a + b

g=fib_gen()
assert [0, 1, 1, 2, 3, 5, 8] == [g.next() for x in range(7)]


def divisible(n, x): return n % x == 0

def divisors(n):
    possible_divisors = range(2, int(sqrt(n))+1)
    return filter(lambda x: divisible(n, x), possible_divisors)

assert divisors(24) == [2, 3, 4]

def is_prime(n):
    # imperative version avoids work, is this
    # possible with lazy seqs?
    return len(divisors(n)) == 0

assert is_prime(37)
assert not is_prime(38)


def lazy_filter(pred, gen):
    while True:
        x = gen.next()
        if pred(x):
            yield x

assert 13 == lazy_filter(lambda x: x > 10, fib_gen()).next()

def take(gen, N):
    "take N elements from an infinite generatpr, filtered by pred"
    found = []
    while len(found) < N:
        found.append(gen.next())
    return found

assert [13, 89, 233] == take(lazy_filter(lambda x: x > 10 and is_prime(x), fib_gen()), 3)


pred = lambda x: x > 227000 and is_prime(x)
X = take(lazy_filter(pred, fib_gen()), 1)[0]
print sum(filter(is_prime, divisors(X+1)))
