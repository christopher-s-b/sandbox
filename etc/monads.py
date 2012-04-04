
# traditional, code-based error handling (no exceptions)


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
    if not mUserid: return None

    mBalance = balance_from_userid(mUserid)
    if not mUserid: return None

    mLoan = balance_qualifies_for_loan(mBalance)
    if not mUserid: return None

    return mLoan



for person_name in ["Irek", "John", "Alex", "Nick", "Fake"]:
    qualified = name_qualifies_for_loan(person_name)
    print "%s: %s" % (person_name, qualified)
print




# factor out the error checking with a monad

def bind(mv, mf):
    "function invocation, but with plumbing"
    if (mv): return mf(mv)
    else: return None

def lift(value):
    "takes a simple value and turns it into a monadic value (for use with bind)"
    return value

def name_qualifies_for_loan(person_name):
    "note pattern of lift-bind-bind-bind, we can abstract further with macros"
    mName =    lift(person_name)
    mUserid =  bind(mName, userid_from_name)
    mBalance = bind(mUserid, balance_from_userid)
    mLoan =    bind(mBalance, balance_qualifies_for_loan)

    return mLoan


for person_name in ["Irek", "John", "Alex", "Nick", "Fake"]:
    qualified = name_qualifies_for_loan(person_name)
    print "%s: %s" % (person_name, qualified)
print


# add error codes to the plumbing



# helpers for returning error codes
def success(x): return (True, x)
def fail(x): return (False, x)

def userid_from_name(person_name):
    if person_name == "Irek": return success(1)
    elif person_name == "John": return success(2)
    elif person_name == "Alex": return success(3)
    elif person_name == "Nick": return success(1)
    else: return fail("No account associated with name '%s'" % person_name)

def balance_from_userid(userid):
    if userid == 1: return success(1000000)
    elif userid == 2: return success(75000)
    else: return fail("No balance associated with account #%s" % userid)

def balance_qualifies_for_loan(balance):
    if balance > 200000: return success(balance)
    else: return fail("Insufficient funds for loan, current balance is %s" % balance)


# bind knows how to unwrap the return value and pass it to
# the next function
def bind(mv, mf):
    succeeded = mv[0]
    value = mv[1]

    if (succeeded): return mf(value)
    else: return mv

def lift(val): return success(val)

#same monadic code as before

for person_name in ["Irek", "John", "Alex", "Nick", "Fake"]:
    qualified = name_qualifies_for_loan(person_name)
    print "%s: %s" % (person_name, qualified)
