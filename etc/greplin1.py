
def check(str, pivot, depth=1):
    try:
        if str[pivot-depth] == str[pivot+depth]:
            return check(str, pivot, depth+1)
        else:
            return depth
    except IndexError: return depth # or "zero-pad" input

def palindromes(str):
    check_str = lambda pivot: check(str, pivot) # curried
    pivots = range(len(s))
    depths = zip(pivots, map(check_s, pivots))
    found = filter(lambda pair: pair[1]>1, depths)

    def extract(pair):
        pivot = pair[0]
        depth = pair[1]
        return str[pivot-depth+1:pivot+depth]

    return map(extract, found)

sort_len_reverse = lambda str: -1*len(str)

#s="I like racecars that go fast"
s = open('greplin1.txt').read()
print sorted(palindromes(s), key=sort_len_reverse)[0]
