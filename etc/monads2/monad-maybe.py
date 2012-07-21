

def get_account(name):
    if name == "Irek": return 1
    elif name == "John": return 2
    else: return None

def get_balance(account):
    if account == 1: return 1000000
    elif account == 2: return 75000
    else: return None

def get_qualified_amount(balance):
    if balance > 200000: return balance
    else: return None


def get_loan(name):
    account = get_account(name)
    if not account:
        return None
    balance = get_balance(account)
    if not balance:
        return None
    loan = get_qualified_amount(balance)
    return loan

#print get_loan("Irek") => 1000000
#print get_loan("John") => None

#print get_qualified_amount(get_balance(get_account("Irek"))) => 1000000
#print get_qualified_amount(get_balance(get_account("John"))) => TypeError

def bind(v, f):
    if (v):
        return f(v)
    else:
        return None

def get_loan(name):
    m_account = get_account(name)
    m_balance = bind(m_account, get_balance)
    m_loan =    bind(m_balance, get_qualified_amount)
    return m_loan

def m_chain(val, fns):
    m_val = val
    for f in fns:
        m_val = bind(m_val, f)
    return m_val

#print m_chain("Irek", [get_account, get_balance, get_qualified_amount]) # => 1000000
#print m_chain("John", [get_account, get_balance, get_qualified_amount]) # => None

def m_chain2(val, *fns):
    return m_chain(val, fns)

#print m_chain2("Irek", get_account, get_balance, get_qualified_amount) # => 1000000
#print m_chain2("John", get_account, get_balance, get_qualified_amount) # => None

#from functional import partial, compose
#multi_compose = partial(reduce, compose)
