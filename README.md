erlang-servers
==============

NashFP "Build an Erlang Server" lab materials

For the lab we will use the example of a bank ATM. 

The ATM supports multiple accounts and maintains a balance. 

super simple ATM
----------------
We will start with a super simple server. It will help everyone get comfortable compiling (and running) code. It will also show the idea of a server loop, messages and a receive block.

This super-simple ATM will support the following actions. An account is created by deposting money to it.

{check_balance, AccountNumber} -> {ok, Amount} ; {no_such_account}

{withdraw, AccountNumber, Amout} -> {ok, NewBalance}; {overdrawn}; {no_such_account}

{deposit, AccountNumber}-> {ok, NewBalance}

To start we won't bother with PIN numbers.


