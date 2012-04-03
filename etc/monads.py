

def bind(mv, mf):
    if (mv): return mf(mv)
    else: return None

def result(mv):
    return mv

def yes(x): return x

def no(x): return None


def userid_from_name(person_name):
    if person_name == "Irek": return yes(1)
    elif person_name == "John": return yes(2)
    elif person_name == "Alex": return yes(3)
    elif person_name == "Nick": return yes(1)
    else: return no("No account associated with name %s" % person_name)

def balance_from_userid(userid):
    if userid == 1: return yes(1000000)
    elif userid == 2: return yes(75000)
    else: return no("no balance associated with account #%s" % userid)

def balance_qualifies_for_loan(balance):
    if balance > 200000: return yes(balance)
    else: return no("insufficient funds for loan, current balance is %s" % balance)

def name_qualifies_for_loan(person_name):

    mUserid = userid_from_name(person_name)
    mBalance = bind(mUserid, balance_from_userid)
    mLoan = bind(mBalance, balance_qualifies_for_loan)

    return result(mLoan)


for person_name in ["Irek", "John", "Alex", "Nick", "Dustin"]:
    print "%s: %s" % (person_name, name_qualifies_for_loan(person_name))
