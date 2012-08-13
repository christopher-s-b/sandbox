// from < http://news.ycombinator.com/item?id=1275860 >

    function bind(mv, mf) {
        return function(k, end) {
            return mv(function(result) { return mf(result)(k, end); }, end);
        }
    }

    function yes(x) {
        return function(k, end) { return k(x); };
    }

    function no(x) {
        return function(k, end) { return end(x); };
    }


    function userid_from_name(person_name) {
        switch (person_name) {
            case "Irek": return yes(1); // we have three user names in our system
            case "John": return yes(2);
            case "Alex": return yes(3);
            case "Nick": return yes(1); // Nick is on the same acct as Irek
            default: return no("No account associated with name " + person_name);
        }
    }

    function balance_from_userid(userid) {
        switch (userid) {
            case 1: return yes(1000000); // some amounts for a couple of accounts
            case 2: return yes(75000);
            default: return no("No balance associated with account #" + userid);
        }
    }

    function balance_qualifies_for_loan(balance) {
        if (balance > 200000) return yes(balance);
        else return no("Insufficient funds for loan, current balance is " + balance);
    }

    function name_qualifies_for_loan(person_name) {

        var on_yes = function(x) { return "This person qualifies for a loan. Their account has a balance of: " + x; };
        var on_no  = function(x) { return "Do not issue loan, reason given: " + x; };

        // attempting to express business logic as a series of composable operations, like pipes
        //   person_name | userid_from_name | balance_from_userid | balance_qualifies_for_loan
        // even though each step needs "plumbing" to handle failures.

        // all of these are "monadic values", evaluate them to get at their wrapped value
        var mUserid = userid_from_name(person_name);              // mUserid(on_yes, on_no);
        var mBalance = bind(mUserid, balance_from_userid);        // mBalance(on_yes, on_no);
        var mLoan = bind(mBalance, balance_qualifies_for_loan);   // mLoan(on_yes, on_no);

        // evaluate the chain
        return mLoan(on_yes, on_no);
    }


> name_qualifies_for_loan("Irek");
"This person qualifies for a loan. Their account has a balance of:
1000000"

> name_qualifies_for_loan("John");
"Do not issue loan, reason given: Insufficient funds for loan, current
balance is 75000"

> name_qualifies_for_loan("Alex");
"Do not issue loan, reason given: No balance associated with account #3"

> name_qualifies_for_loan("Nick");
"This person qualifies for a loan. Their account has a balance of:
1000000"

> name_qualifies_for_loan("Foo");
"Do not issue loan, reason given: No account associated with name Foo"



function bind(mv, mf) {
    return (mv != null) ? mf(mv) : null;
}

function yes(x) {
    return x;
}

function no(x) {
    return null;
}





function bind(mv, mf) {
    var succeeded = mv[0];
    var value = mv[1];
    return (succeeded) ? mf(value) : mv;
}

function yes(x) {
    return [true, x]; // pair [succeeded, value]
}

function no(x) {
    return [false, x];
}
