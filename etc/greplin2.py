from math import sqrt

def fib_gen():
    a, b = 0, 1
    while 1:
        yield a
        a, b = b, a + b

def is_prime(n):
    # imperative version avoids work, is this
    # possible with lazy seqs?
    return len(divisors(n)) == 0

def divisors(n):
    possible_divisors = range(2, int(sqrt(n))+1)
    remainders = map(lambda x: n % x, possible_divisors)
    tuples = filter(lambda x: x[1]==0, zip(possible_divisors, remainders))
    
    if len(tuples) == 0:
        return []
    else:
        divisors, remainders = zip(*tuples)
        return divisors

assert divisors(24) == (2, 3, 4)

def take_filter(pred, gen, N):
    "take N elements from an infinite generatpr, filtered by pred"
    found = []
    while len(found) < N:
        x = gen.next()
        if pred(x):
            found.append(x)
    return found


pred = lambda x: x > 227000 and is_prime(x)
#pred = lambda x: x > 10 and is_prime(x)
X = take_filter(pred, fib_gen(), 1)[0]
print sum(filter(is_prime, divisors(X+1)))
