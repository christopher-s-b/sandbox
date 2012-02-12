
def check(str, pivot, depth=1):
    try:
        if str[pivot-depth] == str[pivot+depth]:
            return check(str, pivot, depth+1)
        else:
            return depth
    except IndexError: return depth # or "zero-pad" input

def palindromes(str):
    "doesn't detect even-length palindromes, oops."
    check_str = lambda pivot: check(str, pivot) # curried
    pivots = range(len(s))
    depths = zip(pivots, map(check_str, pivots))
    found = filter(lambda pair: pair[1]>1, depths)

    def extract(pair):
        pivot = pair[0]
        depth = pair[1]
        return str[pivot-depth+1:pivot+depth]

    return map(extract, found)

sort_len_reverse = lambda str: -1*len(str)

assert "racecar" == sorted(palindromes("I like racecars that go fast"), key=sort_len_reverse)[0]
#assert "asdffdsa" == sorted(palindromes("I like rac asdffdsaecars that go fast"), key=sort_len_reverse)[0]

s = open('greplin1b.txt').read()
print sorted(palindromes(s), key=sort_len_reverse)[0]
