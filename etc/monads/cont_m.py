

def success(x):
    def _make_continue(k_continue, k_fail):
        return k_continue(x)
    return _make_continue

def error(x):
    def _make_fail(k_continue, k_fail):
        return k_fail(x)
    return _make_fail



def get_account(name):
    if name == "Irek": return success(1)
    elif name == "John": return success(2)
    elif name == "Alex": return success(3)
    elif name == "Nick": return success(1)
    else: return error("No account associated with name '%s'" % name)

def get_balance(account):
    if account == 1: return success(1000000)
    elif account == 2: return success(75000)
    else: return error("No balance associated with account #%s" % account)

def qualified_amount(balance):
    if balance > 200000: return success(balance)
    else: return error("Insufficient funds for loan, current balance is %s" % balance)







def bind(mval, mf):
    def _continue(k, end): # named right?
        return mval(lambda result: mf(result)(k, end), end)
    return _continue

def unit(x): return success(x)


def get_loan(name):

    mval = bind(unit(name), lambda name:
           bind(get_account(name), lambda account:
           bind(get_balance(account), lambda balance:
                    qualified_amount(balance))))


    on_qualified = lambda loan: "qualified for amount: %s" % loan
    on_disqualified = lambda why: "not qualified for loan, reason given: %s" % why
    return mval(on_qualified, on_disqualified)

names = ["Irek", "John", "Alex", "Fred"]
loans = map(get_loan, names)
for name, loan in zip(names, loans):
    print "%s: %s" % (name, loan)
