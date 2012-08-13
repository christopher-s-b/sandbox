import operator as op
from monads import identity_m, error_m

def head(seq): return seq[0]
def tail(seq): return seq[1:]


def err(msg): return None, msg
def ok(val): return val, None

class Symbol: pass
class Literal: pass
class Func: pass
class Sexpr: pass

def sym(tok): return (Symbol, tok)
def lit(tok): return (Literal, tok)
def sexpr(tok): return (Sexpr, tok)
def func(tok): return (Func, tok)


interpreter_m = identity_m
domonad = lambda mf, mv: interpreter_m.bind(mv, mf)


env = {
    'x': (Literal, 1),
    '+': (Func, op.add)
}

def eval1(form):
    if form[0] == Literal:
        return ok(form)
    elif form[0] == Symbol:
        sym = form[1]
        return ok(env[sym]) if sym in env else err("symbol `%s` undefined" % sym)
    elif form[0] == Sexpr:
        print form[1]
        return evalS(form[1])
    else: assert False, "unsupported form type"

def evalS(sexpr):
    """recursively evaluate the form until there are no terms left"""
    form, forms = head(sexpr), tail(sexpr)
    fn = eval1(form)
    if not fn[0] == Func:
        return err("form in sexpr head position must resolve to a func")
    return apply(fn, forms)


def apply(fn, args):
    args = [a[1] for a in interpreter_m.map(eval1, args)] #unpack the tuples
    # args are allowed to be Literals or Fns (have to be native python types now)
    return ok(Literal, fn[1](*args))


assert eval1(lit(1)) == ok((Literal, 1))
assert eval1(sym("x")) == ok((Literal, 1))
assert eval1(sym("+")) == ok((Func, op.add))
assert eval1(sym("y")) == err('symbol `y` undefined')

assert eval1(sexpr([sym("+"), sym("x"), lit(3)])) == (Literal, 4)
assert evalS([sym("+"), sym("x"), lit(3)]) == (Literal, 4)

assert eval1(
         sexpr([sym("+"), sym("x"),
           sexpr([sym("+"), lit(42), lit(1)])
           ])) == (Literal, 44)




def eval_a(form):
    return identity_m.bind(form, eval1)

def eval_b(form):
    return error_m.bind(form, eval1)

# "(\x -> + x 1) 3"
#Apply(Lambda("x", Expr("+", "x", "1")), "3")
