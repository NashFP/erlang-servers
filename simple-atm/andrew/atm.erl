-module(atm).

-export([start/0, init/0, stop/0, check_balance/1, withdraw/2, deposit/2]).

start() ->
    case (whereis(atm)) of
	undefined -> 
	    register(atm, spawn(atm, init, [])),
	    started;
	_ ->
	    started
    end.

init() ->
    Accounts = dict:new(),
    loop(Accounts).

stop() ->
    call(stop).

check_balance(AccountNumber) ->
    call({check_balance, AccountNumber}).

withdraw(AccountNumber, Amount) ->
    call({withdraw, AccountNumber, Amount}).

deposit(AccountNumber, Amount) ->
    call({deposit, AccountNumber, Amount}).


call(Message) ->

    case (whereis(atm)) of
	undefined -> 
	    atm_closed;
	_ ->
	    atm ! {request, self(), Message},
	    receive
		{reply, Reply} -> Reply
	    end
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.


loop(Accounts) ->
    receive 
	{request, Pid, {check_balance, AccountNumber}} ->
	    {NewAccounts, Reply} = check_balance(Accounts, AccountNumber),
	    reply(Pid, Reply),
	    loop(NewAccounts);
	{request, Pid, {withdraw, AccountNumber, Amount}}  ->
	    {NewAccounts, Reply} = withdraw(Accounts, AccountNumber, Amount),
	    reply(Pid, Reply),
	    loop(NewAccounts);
	{request, Pid, {deposit, AccountNumber, Amount}} ->
	    {NewAccounts, Reply} = deposit(Accounts, AccountNumber, Amount),
	    reply(Pid, Reply),
	    loop(NewAccounts);
	{request, Pid, stop} ->
	    io:format("Shutting down...~n"),
	    reply(Pid, stopped);
	Unknown -> 
	    io:format("Unknown message: ~p~n", [Unknown]),
	    loop(Accounts)
    end.

check_balance(Accounts, AccountNumber) ->
    case (dict:find(AccountNumber, Accounts)) of 
	error ->
	    {Accounts, no_such_account};
	{ok, Balance} ->
	    {Accounts, {balance, lists:sum(dict:fetch(AccountNumber, Accounts))}}
    end.


withdraw(Accounts, AccountNumber, Amount) ->
    case (dict:find(AccountNumber, Accounts)) of 
	error ->
	    {Accounts, no_such_account};
	{ok, Balance} ->
	    NewBalance = lists:sum(dict:fetch(AccountNumber, Accounts)),
	    case (NewBalance < 0) of
		true ->
		    {Accounts, overdrawn};
		_ ->
		    NewAccounts = dict:store(AccountNumber, dict:fetch(AccountNumber, Accounts) ++ [-Amount], Accounts), 
		    {NewAccounts, {new_balance, NewBalance - Amount}}
	    end
    end.

deposit(Accounts, AccountNumber, Amount) ->
    OldBalance =     
	case (dict:find(AccountNumber, Accounts)) of 
	    error ->
			NewAccounts = dict:store(AccountNumber, [Amount], Accounts);
	    {ok, Balance} ->
			NewAccounts = dict:store(AccountNumber, dict:fetch(AccountNumber, Accounts) ++ [Amount], Accounts)
	end,

    {NewAccounts, {new_balance, dict:fetch(AccountNumber, NewAccounts)}}.
