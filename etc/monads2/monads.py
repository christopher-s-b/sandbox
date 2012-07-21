

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


# the following m_* functions are ported from clojure/algo.monads
# https://github.com/clojure/algo.monads/blob/3d7baa96d9435245f98e395bcddae4427eba1a85/src/main/clojure/clojure/algo/monads.clj#L276

def identity(x): return x

def m_join(mv):
    """Converts a monadic value containing a monadic value into a 'simple'
    monadic value."""
    return bind(mv, identity)

def m_fmap(f, mv):
    """Bind the monadic value m to the function returning (f x) for
    argument x"""
    return bind(mv, lambda x: unit(f(x)))


#(defmonadfn m-seq
  #"'Executes' the monadic values in ms and returns a sequence of the
  # basic values contained in them."
#  [ms]
#  (reduce (fn [q p]
#            (m-bind p (fn [x]
#                        (m-bind q (fn [y]
#                                    (m-result (cons x y)))) )))
#          (m-result '())
#          (reverse ms)))

#(defmonadfn m-map
#  [f xs]
#  (m-seq (map f xs)))

def _reverse(xs): return xs[-1::-1]

def m_seq(ms):
    """'Executes' the monadic values in ms and returns a sequence of the
    basic values contained in them."""
    def f(q, p):
        return bind(p, lambda x:
               bind(q, lambda y:
                        unit(x + [y]))) #(cons x y)
    return reduce(f, unit([]), _reverse(ms))

def m_map(f, xs):
    """'Executes' the sequence of monadic values resulting from mapping
    f onto the values xs. f must return a monadic value."""
    return m_seq(map(f, xs))

def m_chain(*fns):
    """returns a function of one argument which performs the monadic
    composition of fns."""
    def m_chain_link(chain_expr, step):
        return lambda v: bind(chain_expr(v), step)
    return reduce(m_chain_link, fns, unit)

def m_reduce(f, mvs):
    """Return the reduction of (m-lift 2 f) over the list of monadic values mvs
    with initial value (m-result val)."""
    pass


def m_until():
    """While (p x) is false, replace x by the value returned by the
    monadic computation (f x). Return (m-result x) for the first
    x for which (p x) is true."""
    pass



"""
(defmonadfn m-reduce
  ([f mvs]
   (if (empty? mvs)
     (m-result (f))
     (let [m-f (m-lift 2 f)]
       (reduce m-f mvs))))
  ([f val mvs]
   (let [m-f    (m-lift 2 f)
         m-val  (m-result val)]
     (reduce m-f m-val mvs))))

(defmonadfn m-until
  [p f x]
  (if (p x)
    (m-result x)
    (domonad
      [y (f x)
       z (m-until p f y)]
      z)))


"""

# can we define m_lift? it's a macro in clojure

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
def _flatten(listOfLists):
    "Flatten one level of nesting"
    return list(chain.from_iterable(listOfLists))

seq_m = {
    'unit': lambda v: [v],
    'bind': lambda mv, mf: _flatten(map(mf, mv))
}


with monad(seq_m):
    assert bind(range(5), lambda x: unit(x*2)) == [0,2,4,6,8]

    # equivalent to [y for x in range(5) for y in range(x)]
    assert m_chain(range, range)(5) == [0, 0, 1, 0, 1, 2, 0, 1, 2, 3]
    assert bind(range(5), range) == [0, 0, 1, 0, 1, 2, 0, 1, 2, 3]


def _chessboard():
    ranks = list("abcdefgh")
    files = list("12345678")

    with monad(seq_m):
        return bind(ranks, lambda rank:
               bind(files, lambda file:
                       unit((rank, file))))

assert len(_chessboard()) == 64
assert _chessboard()[:3] == [('a', '1'), ('a', '2'), ('a', '3')]



# concept of map generalizes to monad operatons that aren't list
# comprehensions:
with monad(maybe_m): assert m_fmap(lambda x:2*x, None) == None
with monad(maybe_m): assert m_fmap(lambda x:2*x, 2) == 4
with monad(seq_m): assert m_fmap(lambda x:2*x, range(5)) == [0,2,4,6,8]
