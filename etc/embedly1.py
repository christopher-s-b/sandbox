# n! means n * (n - 1) * ... * 3 * 2 * 1
# For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800
# Let R(n) equal the sum of the digits in the number n!
# For example, R(10) is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# Find the lowest value for n where R(n) is 8001.

from operator import mul, add

oneTo = lambda n: range(1, n+1)
assert [1,2,3,4,5] == oneTo(5)

fac = lambda n: reduce(mul, oneTo(n), 1)
assert 3628800 == fac(10)

digits = lambda n: map(int, str(n))
assert [1, 2, 3] == digits(123)

R = lambda n: sum(digits(fac(n)))
assert 27 == R(10)

r = range(1,800)
print filter(lambda n: n[0]==8001, zip(map(R, r), r))

