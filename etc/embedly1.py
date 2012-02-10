import math

sum = lambda n: reduce(lambda a,b:a+b, [ int(c) for c in str(math.factorial(n)) ], 0)
r = range(1,800)
sums = map(sum, r)
print filter(lambda n: n[0]==8001, zip(sums,r))


