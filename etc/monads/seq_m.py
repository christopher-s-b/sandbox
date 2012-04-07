

#forget about error handling for now


def success(val): return val, None
def error(why): return None, why

def get_banks(name):
    if name == "Irek": return success(["Bank of America", "Chase"])
    elif name == "John": return success(["PNC Bank", "Wells Fargo"])
    elif name == "Alex": return success(["TD Bank"])
    else: return error("No bank associated with name %s" % name)

def get_accounts(bank, name):
    if   name == "Irek" and bank == "Bank of America": return success([1, 2])
    elif name == "Irek" and bank == "Chase": return success([3])
    elif name == "John" and bank == "PNC Bank": return success([4])
    elif name == "John" and bank == "Wells Fargo": return success([5, 6])
    elif name == "Alex" and bank == "TD Bank": return success([7, 8])
    else: return error("No account associated with (%s, %s)" % (bank, name))

def get_balance(bank, account):
    return 250000

def qualified_amount(balance):
    if balance > 200000: return success(balance)
    else: return error("Insufficient funds for loan, current balance is %s" % balance)


def get_val(m_val): return m_val[0]
def get_error(m_val): return m_val[1]


def get_loan(name):

    m_banks = get_banks(name)
    if get_error(m_banks):
        return m_banks
    banks = get_val(m_banks)

    for bank in banks:
        m_accounts = get_accounts(bank, name)
        if get_error(m_accounts):
            return m_accounts
        accounts = get_val(m_accounts)

        for account in accounts:
            return qualified_amount(get_balance(bank, account))



names = ["Irek", "John", "Alex", "Fred"]
loans = map(get_loan, names)
for name, loan in zip(names, loans):
    print "%s: %s" % (name, loan)

print



# seq monad
def seq_unit(x): return [x]

def seq_bind(mval, mf):
    """unpack a list of values, invoking mf(val) for each value. Each result is
    a list of values, collect these lists into a single list.
    returns a list of results in seq-monad."""

    #print "%s: %s" % (type(mval), mval)
    assert isinstance(mval, list)

    return flatten(map(mf, mval))


#usage of seq monad

ranks = list("abcdefgh")
files = list("12345678")

def chess_squares_1():
    return [ (rank, file)
             for rank in ranks
             for file in files ]

assert len(chess_squares_1()) == 64
assert chess_squares_1()[:3] == [('a', '1'), ('a', '2'), ('a', '3')]


def chess_squares_2():
    # this function will use seq-m
    unit = seq_unit
    bind = seq_bind

    return bind(ranks, lambda rank:
           bind(files, lambda file:
                   unit((rank, file))))

assert len(chess_squares_2()) == 64
assert chess_squares_1() == chess_squares_2()






# error monad
def error_unit(x): return success(x)

def error_bind(mval, mf):
    """unpack monadic value from error monad into (val, error).
    invoke mf(val), but only when there is not an error.
    returns a result in error-monad."""

    #print "%s: %s" % (type(mval), mval)
    assert isinstance(mval, tuple)

    error = get_error(mval)
    value = get_val(mval)
    if error:
        return mval
    return mf(value)

# combined monad !!
def unit(x): return error_unit(seq_unit(x))
def bind(mval, mf): return error_bind(mval, lambda mval: seq_bind(mval, mf))

def get_loan(name):

    return bind(unit(name), lambda name:
           bind(get_banks(name), lambda bank:
           bind(get_accounts(bank, name), lambda account:
                    unit(qualified_amount(get_balance(bank, account))))))


# def get_loan_wrong(name):

#     m_name = unit(name)
#     m_banks = bind(m_name, get_banks)

#     # trouble - get_accounts needs access to the wrapped value name
#     m_accounts = bind(m_banks, get_accounts)

#     # trouble - get_balance needs access to unwrapped values
#     balance = get_balance(bank, account)
#     return qualified_amount(balance)


names = ["Irek", "John", "Alex", "Fred"]
loans = map(get_loan, names)
for name, loan in zip(names, loans):
    print "%s: %s" % (name, loan)

print





#print "Fred: %s" % get_loan_amount("Fred")

# def get_loan_amount(name):
#     return [ qualified_amount(get_balance(bank, account))
#         for bank in get_banks(name)
#         for account in get_accounts(bank, name) ]


# names = ["Irek", "John", "Alex", "Fred"]
# loans = map(get_loan, names)
# for name, loan in zip(names, loans):
#     print "%s: %s" % (name, loan)




# # bind knows how to unwrap the return value and pass it to
# # the next function
# def bind(mv, mf):
#     value = mv[0]
#     error = mv[1]
#     if not error:
#         return mf(value)
#     else:
#         return mv

# def unit(value):
#     return value, None

# def name_qualifies_for_loan(name):
#     m_name =    unit(name)
#     m_userid =  bind(m_name, userid_from_name)
#     m_balance = bind(m_userid, balance_from_userid)
#     m_loan =    bind(m_balance, balance_qualifies_for_loan)
#     return m_loan

# for name in ["Irek", "John", "Alex", "Nick", "Fake"]:
#     qualified = name_qualifies_for_loan(name)
#     print "%s: %s" % (name, qualified)
# print
