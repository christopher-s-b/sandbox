
def mul_of(a,b):
  return 0 == a % b

targets = {3:"fizz", 5:"buzz"}

def fizzbuzz(n):
  matches = filter(lambda i: mul_of(n, i), targets.keys()) #curried
  return str(n) if len(matches) == 0 else "".join(map(lambda i: targets[i], matches))

print ", ".join(map(fizzbuzz, range(20)))
