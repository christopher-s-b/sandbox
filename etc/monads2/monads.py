from __future__ import print_function


def identity(x): return x
def _reverse(xs): return xs[-1::-1]

class Monad:

# the following m_* functions are ported from clojure/algo.monads
# https://github.com/clojure/algo.monads/blob/3d7baa96d9435245f98e395bcddae4427eba1a85/src/main/clojure/clojure/algo/monads.clj#L276

    def m_join(self, mv):
        """Converts a monadic value containing a monadic value into a 'simple'
        monadic value."""
        return self.bind(mv, identity)

    def m_fmap(self, f, mv):
        """Bind the monadic value m to the function returning (f x) for
        argument x"""
        return self.bind(mv, lambda x: self.unit(f(x)))


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



    def m_seq(self, ms):
        """'Executes' the monadic values in ms and returns a sequence of the
        basic values contained in them."""
        def f(q, p):
            return self.bind(p, lambda x:
                   self.bind(q, lambda y:
                            self.unit(x + [y]))) #(cons x y)
        return reduce(f, self.unit([]), _reverse(ms))

    def m_map(self, f, xs):
        """'Executes' the sequence of monadic values resulting from mapping
        f onto the values xs. f must return a monadic value."""
        return self.m_seq(map(f, xs))

    def m_chain(self, *fns):
        """returns a function of one argument which performs the monadic
        composition of fns."""
        def m_chain_link(chain_expr, step):
            return lambda v: self.bind(chain_expr(v), step)
        return reduce(m_chain_link, fns, self.unit)

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
class _Identity_m(Monad):
    def unit(self, v): return v
    def bind(self, mv, mf): return mf(mv)

identity_m = _Identity_m()

identity_m.m_chain(lambda x:2*x, lambda x:2*x)(2) == 8


class _Maybe_m(Monad):
    def bind(self, mv, mf): return mf(mv) if mv else None
    def unit(self, v): return v

maybe_m = _Maybe_m()
assert maybe_m.m_chain(lambda x:2*x, lambda x:2*x)(2) == 8
assert maybe_m.m_chain(lambda x:None, lambda x:2*x)(2) == None

class _Error_m(Monad):
    def bind(self, mv, mf): return mf(mv[0]) if mv[0] else mv
    def unit(self, v): return (v, None)

error_m = _Error_m()

def _test_error_m():
    success = lambda val: unit(val)
    failure = lambda err: (None, err)

    assert error_m.m_chain(lambda x:success(2*x), lambda x:success(2*x))(2) == (8, None)
    assert error_m.m_chain(lambda x:failure("error"), lambda x:success(2*x))(2) == (None, "error")
    assert error_m.m_chain(lambda x:success(2*x), lambda x:failure("error"))(2) == (None, "error")




from itertools import chain
def _flatten(listOfLists):
    "Flatten one level of nesting"
    return list(chain.from_iterable(listOfLists))

class _List_m(Monad):
    def unit(self, v): return [v]
    def bind(self, mv, mf): return _flatten(map(mf, mv))

list_m = _List_m()

assert list_m.bind(range(5), lambda x: list_m.unit(x*2)) == [0,2,4,6,8]

# equivalent to [y for x in range(5) for y in range(x)]
assert list_m.m_chain(range, range)(5) == [0, 0, 1, 0, 1, 2, 0, 1, 2, 3]
assert list_m.bind(range(5), range) == [0, 0, 1, 0, 1, 2, 0, 1, 2, 3]


def _chessboard():
    ranks = list("abcdefgh")
    files = list("12345678")

    return list_m.bind(ranks, lambda rank:
           list_m.bind(files, lambda file:
           list_m.unit((rank, file))))

assert len(_chessboard()) == 64
assert _chessboard()[:3] == [('a', '1'), ('a', '2'), ('a', '3')]



# concept of map generalizes to monad operatons that aren't list
# comprehensions:
assert maybe_m.m_fmap(lambda x:2*x, None) == None
assert maybe_m.m_fmap(lambda x:2*x, 2) == 4
assert list_m.m_fmap(lambda x:2*x, range(5)) == [0,2,4,6,8]



class _Writer_m(Monad):

    def unit(self, v): return (v, [])
    def get_val(self, mv): return mv[0]
    def get_out(self, mv): return mv[1]

    def bind(self, mv, mf):
        val, out = self.get_val(mv), self.get_out(mv)
        r_mv = mf(val)
        r_out = self.get_out(r_mv)
        final_out = out + r_out if r_out else out
        return (self.get_val(r_mv), final_out)

writer_m = _Writer_m()

def test_writer_m():
    def withlog(val, out): return (val, [out])
    def nolog(val): return (val, [])

    def addOne(x):
        x=x+1
        return withlog(x, "x+1==%s"%x)

    assert writer_m.m_chain(addOne, addOne, addOne)(7) == (10, ['x+1==8', 'x+1==9', 'x+1==10'])

    r = writer_m.bind( withlog(7, "init as 7"), lambda x:
        writer_m.bind( withlog(x+1, "+1"), lambda y:
        writer_m.bind( nolog(y), lambda z:
        writer_m.bind( withlog(x+y+z, "sum the steps"), lambda a:
        writer_m.unit( a )))))

    assert r == (23, ['init as 7', '+1', 'sum the steps'])
test_writer_m()


class _Reader_m(Monad):
    def unit(self, v): return lambda env: v
    def bind(self, mv, mf):
        def _(env):
            val = mv(env)
            return mf(val)(env)
        return _

reader_m = _Reader_m()


from collections import namedtuple
def _test_reader_m():
    Env = namedtuple('Env', ['hostname', 'port', 'outfile'])

    hostname = lambda env: env.hostname
    port = lambda env: env.port
    outfile = lambda env: env.outfile

    r = reader_m.bind( hostname, lambda h:
        reader_m.bind( port,     lambda p:
        reader_m.bind( outfile,  lambda o:
        reader_m.unit( [h, p, o] ))))

    env = Env("localhost", 80, "/etc/passwd")
    assert r(env) == ["localhost", 80, "/etc/passwd"]

_test_reader_m()
