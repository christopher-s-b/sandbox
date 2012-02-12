from math import sqrt

def fib_gen():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a + b



def is_prime(n):
    
    #for x in range(2, sqrt(n)+1):
    #    if n % x == 0: return true

    # imperative version avoids work, is this
    # possible with lazy seqs?
    possible_divisors = range(2, int(sqrt(n))+1)
    remainders = map(lambda x: n % x, possible_divisors)
    factors = filter(lambda x: x==0, remainders)
    return len(factors) == 0


def take_filter(pred, gen, N):
    found = []
    while len(found) < N:
        x = gen.next()
        if pred(x):
            found.append(x)
    return found


pred = lambda x: x > 10 and is_prime(x)
print take_filter(pred, fib_gen(), 1)
