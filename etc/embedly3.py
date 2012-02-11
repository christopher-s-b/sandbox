
add = lambda x,y: x+y
countWords = lambda occs: reduce(add, occs, 0)

occs = [2520/(n+1) for n in range(5)] #900
num_words = countWords(occs) 


for guess in range(100):
    count = countWords(occs[:guess])
    if count >= num_words/2: break

print occs
print num_words, num_words/2 
print guess, count
print occs[:guess]

# output:
# [2520, 1260, 840, 630, 504]
# 5754 2877
# 2 3780
# [2520, 1260]
