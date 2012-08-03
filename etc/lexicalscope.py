

def head(seq): return seq[0]
def tail(seq): return seq[1:]


class Expr:
    def __init__(self, *args):
        self.args = args
    def eval(self, env):
        """recursively evaluate the expr until there are no terms left"""
        args = self.args
        term, args = head(args), tail(args)

        if isinstance(term, Lambda):
            rhs, args = head(args), tail(args)
            return term.apply(env, rhs)
        elif isinstance(term, Symbol):
            return val.eval(

        #if f == "+":
        #    lhs, args = head(args), tail(args)
        #    return rhs.eval(env) + lhs.eval(env)
        #return None

    def reduce(self, env):
        if isinstance(self, Literal): return self
        expr = self.eval(env)
        self.reduce(expr, env)

class Lambda:
    def __init__(self, *args):
        pass
    def apply(self, env):


class Symbol(Expr):
    def eval(self, env):
        sym = head(self.args)
        return env[sym]

class Literal(Expr):
    def eval(self, env):
        lit = head(self.args)
        return lit




env = {"x": 3, "addOne": Lambda(
assert Expr("addOne", Symbol("x")).eval({"x": 3}) == 4





# "(\x -> + x 1) 3"
#Apply(Lambda("x", Expr("+", "x", "1")), "3")
