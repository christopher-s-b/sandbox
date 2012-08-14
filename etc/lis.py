################ Lispy: Scheme Interpreter in Python

## (c) Peter Norvig, 2010; See http://norvig.com/lispy.html

################ Symbol, Env classes

from __future__ import division
import math, operator as op

from monads import error_m, err


interp_m = error_m # cont_t state_t error_t identity_m

ok = interp_m.unit
err = err #needs to be lifted if the monad stack changes
bind = interp_m.bind
fmap = interp_m.fmap
seq = interp_m.seq
mmap = interp_m.map


Symbol = str

class Env(dict):
    "An environment: a dict of {'var':val} pairs, with an outer Env."
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms,args))
        self.outer = outer
    def find(self, var):
        "Find the innermost Env where var appears."
        assert (var in self) or self.outer, "unbound var: %s"%var
        return self if var in self else self.outer.find(var)

py_primitive_fns = {
    '+':op.add,
    '-':op.sub,
    '*':op.mul,
    '/':op.div,
    'not':op.not_,
    '>':op.gt,
    '<':op.lt,
    '>=':op.ge,
    '<=':op.le,
    '=':op.eq,
    'equal?':op.eq,
    'eq?':op.is_,
    'length':len,
    'cons':lambda x,y:[x]+y,
    'car':lambda x:x[0],
    'cdr':lambda x:x[1:],
    'append':op.add,
    'list':lambda *x:list(x),
    'list?': lambda x:isa(x,list),
    'null?':lambda x:x==[],
    'symbol?':lambda x: isa(x, Symbol)
}

special_forms = {
    'assert': lambda p, errmsg: ok(None) if p else err(errmsg)
}

def lift(fn): #lift is a misnomer, its not a real monadic lift. what is it?
    "inlining this fn seems to break python?"
    return lambda *args: ok(fn(*args))

def add_globals(env):
    "Add some Scheme standard procedures to an environment."
    env.update(vars(math)) # sin, sqrt, ...
    for sym, fn in py_primitive_fns.iteritems():
        env[sym] = lift(fn)
    for sym, fn in special_forms.iteritems():
        env[sym] = fn
    return env

global_env = add_globals(Env())

isa = isinstance

################ eval

def eval(x, env=global_env):
    "Evaluate an expression in an environment."
    if isa(x, Symbol):             # variable reference
        assert env
        return ok(env.find(x)[x])

    elif not isa(x, list):         # constant literal
        return ok(x)

    elif x[0] == 'quote':          # (quote exp)
        (_, exp) = x
        return ok(exp)

    elif x[0] == 'if':             # (if test conseq alt)
        (_, test, conseq, alt) = x
        def _doif(val):
            return (conseq if val else alt)
        mbranch = fmap(_doif, eval(test, env))
        return bind(mbranch, lambda branch: eval(branch, env))

    elif x[0] == 'set!':           # (set! var exp)
        (_, var, exp) = x
        def _doset(val):
            #assert env.find(var) -- this is a RuntimeError
            assert env
            env.find(var)[var] = val
            return None
        return fmap(_doset, eval(exp, env))

    elif x[0] == 'define':         # (define var exp)
        (_, var, exp) = x
        def _dodefine(val):
            env[var] = val
            return None
        return fmap(_dodefine, eval(exp, env))

    elif x[0] == 'lambda':         # (lambda (var*) exp)
        (_, vars, exp) = x
        return ok(lambda *args: eval(exp, Env(vars, args, env)))

    elif x[0] == 'begin':          # (begin exp*)
        exprs = x[1:]

        # this code is a recursive monad comprehension, it composes the plumbing
        # such that errors short circuit etc, but discards the passed in value
        # and just evaluates the next expression. the return value is monadic -
        # the last expression evaluated.
        #
        # version if there wasn't a monad:
        #
        #   for exp in x[1:]:
        #     val = eval(exp, env)
        #   return val
        #
        def _dobegin(exp, exprs): # -> mv
            if not exprs: return eval(exp, env)
            return bind(eval(exp, env), lambda _: _dobegin(exprs[0], exprs[1:]))
        return _dobegin(exprs[0], exprs[1:])

    else:                          # (proc exp*)
        mvs = map(lambda exp: eval(exp, env), x)
        mproc, margs = mvs.pop(0), mvs
        args = seq(margs)
        return bind(mproc, lambda proc: proc(*args))

################ parse, read, and user interaction

def read(s):
    "Read a Scheme expression from a string."
    return read_from(tokenize(s))

parse = read

def tokenize(s):
    "Convert a string into a list of tokens."
    return s.replace('(',' ( ').replace(')',' ) ').split()

def read_from(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(read_from(tokens))
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)

def atom(token):
    "Numbers become numbers; every other token is a symbol."
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return Symbol(token)

def to_string(exp):
    "Convert a Python object back into a Lisp-readable string."
    return '('+' '.join(map(to_string, exp))+')' if isa(exp, list) else str(exp)

def repl(prompt='lis.py> '):
    "A prompt-read-eval-print loop."
    while True:
        val = eval(parse(raw_input(prompt)))
        if val is not None: print to_string(val)
