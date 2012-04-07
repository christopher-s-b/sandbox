
# traditional, code-based error handling (no exceptions)


def userid_from_name(name):
    if name == "Irek": return 1
    elif name == "John": return 2
    elif name == "Alex": return 3
    elif name == "Nick": return 1
    else: return None

def balance_from_userid(userid):
    if userid == 1: return 1000000
    elif userid == 2: return 75000
    else: return None

def balance_qualifies_for_loan(balance):
    if balance > 200000: return balance
    else: return None

#compose the three steps
def name_qualifies_for_loan(name):
    userid = userid_from_name(name)
    if not userid:
        return None
    balance = balance_from_userid(userid)
    if not balance:
        return None
    loan = balance_qualifies_for_loan(balance)
    if not loan:
        return None
    return loan

names = ["Irek", "John", "Alex", "Nick", "Fake"]
qualified = map(name_qualifies_for_loan, names)
for name, qualified in zip(names, qualified):
    print "%s: %s" % (name, qualified)

print



# factor out the error checking with a monad

def bind(val, f):
    if (val):
        return f(val)
    else:
        return None

def unit(val):
    return val

def name_qualifies_for_loan(name):
    m_name =    unit(name)
    m_userid =  bind(m_name, userid_from_name)
    m_balance = bind(m_userid, balance_from_userid)
    m_loan =    bind(m_balance, balance_qualifies_for_loan)
    return m_loan

for person_name in ["Irek", "John", "Alex", "Nick", "Fake"]:
    qualified = name_qualifies_for_loan(person_name)
    print "%s: %s" % (person_name, qualified)
print


# add error codes to the plumbing

def userid_from_name(person_name):
    if person_name == "Irek": return 1, None
    elif person_name == "John": return 2, None
    elif person_name == "Alex": return 3, None
    elif person_name == "Nick": return 1, None
    else: return None, "No account associated with name '%s'" % person_name

def balance_from_userid(userid):
    if userid == 1: return 1000000, None
    elif userid == 2: return 75000, None
    else: return None, "No balance associated with account #%s" % userid

def balance_qualifies_for_loan(balance):
    if balance > 200000: return balance, None
    else: return None, "Insufficient funds for loan, current balance is %s" % balance


# bind knows how to unwrap the return value and pass it to
# the next function
def bind(mv, mf):
    value = mv[0]
    error = mv[1]
    if not error:
        return mf(value)
    else:
        return mv

def unit(value):
    return value, None

def name_qualifies_for_loan(person_name):
    mName =    unit(person_name)
    mUserid =  bind(mName, userid_from_name)
    mBalance = bind(mUserid, balance_from_userid)
    mLoan =    bind(mBalance, balance_qualifies_for_loan)
    return mLoan

names = ["Irek", "John", "Alex", "Nick", "Fake"]
qualified = map(name_qualifies_for_loan, names)
for name, qualified in zip(names, qualified):
    print "%s: %s" % (name, qualified)

print



def m_map(mf, mvals):
    return map(lambda x: bind(x, mf), mvals)

names = ["Irek", "John", "Alex", "Nick", "Fake"]
m_names = map(unit, names)
m_decisions = m_map(name_qualifies_for_loan, m_names)

for name, m_decision in zip(names, m_decisions):
    print "%s: %s" % (name, m_decision)
