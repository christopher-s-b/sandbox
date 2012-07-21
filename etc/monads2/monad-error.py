def bind(mval, mf):
    value = mval[0]
    error = mval[1]

    if not error:
        return mf(value)
    else:
        return mval

def unit(val): return success(val)

def m_chain(val, fns):
    m_val = unit(val)
    for f in fns:
        m_val = bind(m_val, f)
    return m_val

def m_chain2(val, *fns):
    return m_chain(val, fns)



def success(val): return (val, None)
def failure(err): return (None, err)

def get_account(name):
    if name == "Irek": return success(1)
    elif name == "John": return success(2)
    else: return failure("No account associated with name '%s'" % name)

def get_balance(account):
    if account == 1: return success(1000000)
    elif account == 2: return success(75000)
    else: return failure("No balance associated with account #%s" % account)

def get_qualified_amount(balance):
    if balance > 200000: return success(balance)
    else: return failure("Insufficient funds for loan, current balance is %s" % balance)


#print m_chain2("Irek", get_account, get_balance, get_qualified_amount)
# => (1000000, None)
#print m_chain2("John", get_account, get_balance, get_qualified_amount)
# => (None, 'Insufficient funds for loan, current balance is 75000')
