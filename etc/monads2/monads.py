

class monad:
    """Effectively, put the monad definition in lexical scope.
    Can't modify the execution environment `globals()` directly, because
    after globals().clear() you can't do anything.
    """
    def __init__(self, monad):
        self.monad = monad
        self.oldglobals = {}

    def __enter__(self):
        for k in self.monad:
            if k in globals(): self.oldglobals[k]=globals()[k]
            globals()[k]=self.monad[k]

    def __exit__(self, type, value, traceback):
        """careful to distinguish between None and undefined.
        remove the values we added, then restore the old value only
        if it ever existed"""
        for k in self.monad: del globals()[k]
        for k in self.oldglobals: globals()[k]=self.oldglobals[k]


def m_chain(*fns):
    """returns a function of one argument which performs the monadic
    composition of fns.

    ported from:
    https://github.com/clojure/algo.monads/blob/3d7baa96d9435245f98e395bcddae4427eba1a85/src/main/clojure/clojure/algo/monads.clj#L276"""
    def m_chain_link(chain_expr, step):
        return lambda v: bind(chain_expr(v), step)
    return reduce(m_chain_link, fns, unit)




identity_m = {
    'bind':lambda v,f:f(v),
    'unit':lambda v:v
}

with monad(identity_m):
    assert m_chain(lambda x:2*x, lambda x:2*x)(2) == 8


maybe_m = {
    'bind':lambda v,f:f(v) if v else None,
    'unit':lambda v:v
}

with monad(maybe_m):
    assert m_chain(lambda x:2*x, lambda x:2*x)(2) == 8
    assert m_chain(lambda x:None, lambda x:2*x)(2) == None


error_m = {
    'bind':lambda mv, mf: mf(mv[0]) if mv[0] else mv,
    'unit':lambda v: (v, None)
}

with monad(error_m):
    success = lambda val: unit(val)
    failure = lambda err: (None, err)

    assert m_chain(lambda x:success(2*x), lambda x:success(2*x))(2) == (8, None)
    assert m_chain(lambda x:failure("error"), lambda x:success(2*x))(2) == (None, "error")
    assert m_chain(lambda x:success(2*x), lambda x:failure("error"))(2) == (None, "error")




from itertools import chain
def flatten(listOfLists):
    "Flatten one level of nesting"
    return list(chain.from_iterable(listOfLists))

list_m = {
    'unit': lambda v: [v],
    'bind': lambda mv, mf: flatten(map(mf, mv))
}


def chessboard():
    ranks = list("abcdefgh")
    files = list("12345678")

    with monad(list_m):
        return bind(ranks, lambda rank:
               bind(files, lambda file:
                       unit((rank, file))))

assert len(chessboard()) == 64
assert chessboard()[:3] == [('a', '1'), ('a', '2'), ('a', '3')]
