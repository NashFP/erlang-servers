-module(atm).

-export([start/0, stop/0, loop/0]).


start() ->
	Pid = spawn(atm, loop, []),
	register(atm, Pid).

stop() ->
	atm ! stop.

loop() ->
	receive
		hi ->
			io:format("Hi!~n"),
			loop();
		stop ->
			io:format("Shutting down!~n");
		Unknown ->
			io:format("Unknown message: ~p~n", [Unknown]),
			loop()
	end.




%% atm:start() -> 
%%     started

%% atm:check_balance(AccountNumber) -> 
%%     {balance, Amount} | no_such_account | atm_closed

%% atm:withdraw(AccountNumber, Amount) -> 
%%     {new_balance, Amount} | overdrawn | no_such_account | atm_closed

%% atm:deposit(AccountNumber, Amount) -> 
%%     {new_balance, Amount} | atm_closed

%% atm:stop() -> 
%%     stopped | atm_closed
