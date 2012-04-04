

def bind(mv, mf):
    if (mv): return mf(mv)
    else: return None

def result(mv):
    return mv




def userid_from_name(person_name):
    if person_name == "Irek": return 1
    elif person_name == "John": return 2
    elif person_name == "Alex": return 3
    elif person_name == "Nick": return 1
    else: return None

def balance_from_userid(userid):
    if userid == 1: return 1000000
    elif userid == 2: return 75000
    else: return None

def balance_qualifies_for_loan(balance):
    if balance > 200000: return balance
    else: return None

def name_qualifies_for_loan(person_name):

    mUserid = userid_from_name(person_name)
    mBalance = bind(mUserid, balance_from_userid)
    mLoan = bind(mBalance, balance_qualifies_for_loan)

    return result(mLoan)




for person_name in ["Irek", "John", "Alex", "Nick", "Fake"]:
    qualified = name_qualifies_for_loan(person_name)
    print "%s: %s" % (person_name, qualified)
