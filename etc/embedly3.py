
occs = [2520/(n+1) for n in range(900)]
assert 900 == len(occs)
assert [2520, 1260, 840, 630, 504] == occs[:5]

num_words = sum(occs)

for guess in range(100):
    count = sum(occs[:guess])
    if count >= num_words/2: break

assert 21 == len(occs[:guess])
