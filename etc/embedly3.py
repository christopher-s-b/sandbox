occs = [2520/(n+1) for n in range(900)]
num_words = reduce(lambda x,y: y+x, occs, 0)

for guess in range(1,101):
    count = reduce(lambda x,y: y+x, occs[:guess], 0)
    if count >= num_words/2: break

print guess, count # 21 9184
