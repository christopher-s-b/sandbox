from math import sqrt

def divisible(n, x): return n % x == 0

def divisors(n):
    possible_divisors = range(2, int(sqrt(n))+1)
    return filter(lambda x: divisible(n, x), possible_divisors)

assert divisors(24) == [2, 3, 4]


def is_prime(n):
    return len(divisors(n)) == 0

assert is_prime(37)
assert not is_prime(38)


def primes(n):
    return filter(is_prime, range(1, n))

assert primes(20) == [1, 2, 3, 5, 7, 11, 13, 17, 19]
