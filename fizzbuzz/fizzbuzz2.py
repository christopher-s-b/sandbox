
def mul_of(a,b):
  return 0 == a % b

targets = {3:"fizz", 5:"buzz"}

def fizzbuzz(n):
  matches = filter(targets.keys(), lambda i: mul_of(n, i)) #curried
  return str(n) if len(matches) == 0 else "".join(map(matches, lambda i: targets[i]))

", ".join(map(range(20), fizzbuzz))
