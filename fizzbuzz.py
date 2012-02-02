for n in range(20):
  div_3 = n%3==0
  div_5 = n%5==0

  if div_3: print "fizz",
  elif div_5: print "buzz",
  elif div_3 and div_5: print "fizzbuzz",
  else: print n,

  print

