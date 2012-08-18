def fib1(n):
    if n <= 1: return n
    return fib1(n-1) + fib1(n-2)

assert map(fib1, range(10)) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
