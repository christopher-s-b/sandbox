
def m_map(mf, mvals):
    return map(lambda x: bind(x, mf), mvals)

names = ["Irek", "John", "Alex", "Nick", "Fake"]
m_names = map(unit, names)
m_decisions = m_map(name_qualifies_for_loan, m_names)

for name, m_decision in zip(names, m_decisions):
    print "%s: %s" % (name, m_decision)
