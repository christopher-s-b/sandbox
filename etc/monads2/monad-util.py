

def m_chain(monad, *fns):
    bind = monad['bind']; unit = monad['unit']
    def m_chain_link(chain_expr, step):
        return lambda v: bind(chain_expr(v), step)
    return reduce(m_chain_link, fns, unit)

assert m_chain(identity_m, lambda x:2*x, lambda x:2*x)(2) == 8
assert m_chain(maybe_m, lambda x:None, lambda x:2*x)(2) == None


def m_chain(*fns, **monad):
    bind = monad['bind']; unit = monad['unit']
    def m_chain_link(chain_expr, step):
        return lambda v: bind(chain_expr(v), step)
    return reduce(m_chain_link, fns, unit)

assert m_chain(lambda x:2*x, lambda x:2*x, **identity_m)(2) == 8
assert m_chain(lambda x:None, lambda x:2*x, **maybe_m)(2) == None



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
    def m_chain_link(chain_expr, step):
        return lambda v: bind(chain_expr(v), step)
    return reduce(m_chain_link, fns, unit)

with monad(identity_m):
    assert m_chain(lambda x:2*x, lambda x:2*x)(2) == 8

with monad(maybe_m):
    assert m_chain(lambda x:None, lambda x:2*x)(2) == None
