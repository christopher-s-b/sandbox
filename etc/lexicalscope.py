import operator as op

def head(seq): return seq[0]
def tail(seq): return seq[1:]

def eval1(form):
    if form[0] == Literal:
        return form
    elif form[0] == Symbol:
        if form[1] == "+": return func(op.add)
        elif form[1] == "x": return lit(1)
        else: assert False, "unsupported symbol `%s`" % form[1]
    elif form[0] == Sexpr:
        return evalS(form[1])
    else: assert False, "unsupported form type"

def evalS(sexpr):
    """recursively evaluate the form until there are no terms left"""
    form, forms = head(sexpr), tail(sexpr)
    fn = eval1(form)
    assert fn[0] == Func, "form in sexpr head position must resolve to a func"
    return apply(fn, forms)


def apply(fn, args):
    args = [a[1] for a in map(eval1, args)] #unpack the tuples
    # args are allowed to be Literals or Fns (have to be native python types now)
    return Literal, fn[1](*args)

class Symbol: pass
class Literal: pass
class Func: pass
class Sexpr: pass

def sym(tok): return (Symbol, tok)
def lit(tok): return (Literal, tok)
def sexpr(tok): return (Sexpr, tok)
def func(tok): return (Func, tok)

assert eval1(lit(1)) == (Literal, 1)
assert eval1(sym("x")) == (Literal, 1)
assert eval1(sym("+")) == (Func, op.add)

assert eval1(sexpr([sym("+"), sym("x"), lit(3)])) == (Literal, 4)
assert evalS([sym("+"), sym("x"), lit(3)]) == (Literal, 4)

assert eval1(
         sexpr([sym("+"), sym("x"),
           sexpr([sym("+"), lit(42), lit(1)])
           ])) == (Literal, 44)


# "(\x -> + x 1) 3"
#Apply(Lambda("x", Expr("+", "x", "1")), "3")
