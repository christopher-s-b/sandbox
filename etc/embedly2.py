from math import sqrt


# printing visitor for diagnostics
def pp(x):
    print x
    return x

def stdev(col):
    N = len(col)
    mean = sum(col)/N
    return sqrt(sum([(x-mean)**2 for x in col])/N)

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

    print stdev(depths)


from lxml import etree
f = open('embedly2.html')
dom = etree.XML(f.read())
doit(dom)
