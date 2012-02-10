
def pp(x):
    print x
    return x

def meanstdv(x):
    from math import sqrt
    n, mean, std = len(x), 0, 0
    for a in x:
        mean = mean + a
    mean = mean / float(n)
    for a in x:
        std = std + (a - mean)**2
    std = sqrt(std / float(n-1))
    return mean, std


def traverse(visit, tree, depth=0):
    for child in tree:
        visit(child, depth)
        traverse(visit, child, depth+1)

def doit(tree):
    
    els = []
    visit = lambda tree, depth: els.append((tree, depth))
    traverse(visit, tree)

    ps = filter(lambda x: x[0].tag=='p', els)
    depths = [x[1] for x in ps]

    mean, stdev = meanstdv(depths)
    print mean, stdev


from lxml import etree
f = open('embedly2.html')
dom = etree.XML(f.read())
doit(dom)
