-module(test).
-compile(export_all).

calc_fib(Pid, N, Times) ->
	[fib_service:calc_cast(Pid, N) || _ <- lists:seq(1, Times)],
	[ begin
		receive
			{_Ref, _Reply} -> ok end
	  end || _ <- lists:seq(1, Times)].

call_service(Workers, N, Times) ->
	Pid = fib_service:start(Workers),
	{Cost, _} = timer:tc(fun() -> calc_fib(Pid, N, Times) end),
	Cost.

test() ->
	fib_service:start()
	Times = 10,
	N = 35, 
	[ io:format("workers:~p, cost:~p~n", [Workers, call_service(Workers, N, Times)]) 
		|| Workers <- [1,2,4,10]  ].







