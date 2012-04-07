

def success(val): return val, None
def error(why): return None, why



def get_banks(name):
    if name == "Irek": return success(["Bank of America", "Wells Fargo"])
    elif name == "John": return success(["PNC Bank"])
    elif name == "Alex": return success(["TD Bank"])
    else: return error("No bank associated with name %s" % name)

def get_accounts(bank, name):
    if   name == "Irek" and bank == "Bank of America": return success([1, 2])
    elif name == "Irek" and bank == "Wells Fargo": return success([3])
    elif name == "John" and bank == "PNC Bank": return success([4])
    elif name == "John" and bank == "Wells Fargo": return success([5, 6])
    elif name == "Alex" and bank == "TD Bank": return success([7, 8])
    else: return error("No account associated with (%s, %s)" % (bank, name))

def get_balance(bank, account):
    if bank == "Wells Fargo":
        return error("Unable to get balance due to technical issue for %s: %s" % (bank, account))
    else:
        return success([account * 35000])  #right around 200,000 depending on acct number

def get_qualified_amount(balance):
    if balance > 200000:
        return success([balance])
    else:
        return error("Insufficient funds for loan, current balance is %s" % balance)




def get_value(m_val): return m_val[0]
def get_error(m_val): return m_val[1]

# error monad
def error_unit(x): return success(x)
def error_bind(mval, mf):
    assert isinstance(mval, tuple)
    error = get_error(mval)
    if error: return mval
    else: return mf(get_value(mval))



def flatten(listOfLists):
    "Flatten one level of nesting"
    return [x for sublist in listOfLists for x in sublist]


# sequence monad
def seq_unit(x): return [x]
def seq_bind(mval, mf):
    assert isinstance(mval, list)
    return flatten(map(mf, mval))

# combined monad !!
def unit(x): return error_unit(seq_unit(x))
def bind(mval, mf):
    return error_bind(mval, lambda mval: seq_bind(mval, mf))
    #return seq_bind(mval, lambda mval: error_bind(mval, mf))



# monadic business logic
def get_loan(name):

    m_qualified_amounts = (
           bind(get_banks(name), lambda bank:
           bind(get_accounts(bank, name), lambda account:
           bind(get_balance(bank, account), lambda balance:
           bind(get_qualified_amount(balance), lambda qualified_amount:
                    unit(qualified_amount))))))

#    print m_qualified_amounts, type(m_qualified_amounts)
    #assert isinstance(m_qualified_amounts, list)
    #assert isinstance(m_qualified_amounts, tuple)
    return m_qualified_amounts




names = ["Irek", "John", "Alex", "Fred"]
for name, loans in zip(names, map(get_loan, names)):
    #print "%s: %s: %s" % (name, type(loans), [type(loan) for loan in loans])
    print "%s: %s" % (name, loans)
    #print
