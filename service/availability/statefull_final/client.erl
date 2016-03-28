-module(client).
-compile(export_all).

test_availability() ->
	io:format("test availability begin .....................................~n"),
	test(0).

test(N) ->
	spawn(fun() -> 
		op_api:begin_trans(N, 4),
		op_api:op(N, double),
		op_api:op(N, fib),
		{ok, 21} = op_api:end_trans(N),
		io:format("test round ~p ok.~n", [N])
	  end),
	timer:sleep(100),
	test(N + 1).

test_fib(UserID, N, Expected) ->
	TimeOut = 10 * 60 * 1000,
	op_api:begin_trans(UserID, N, TimeOut),
	op_api:op(UserID, fib, TimeOut),
	{ok, Expected} = op_api:end_trans(UserID, TimeOut),
	ok.

test_scalability() ->
	io:format("test scalability begin .....................................~n"),
	N = 35, 
	[ spawn(fun() -> 
				Cost = timer:tc(fun() -> test_fib(UserID, N, fib_iter(N)) end),
				io:format("test round ~p cost:~p~n", [UserID, Cost])
	  end) || UserID <- lists:seq(1, 8)].

test_softrealtime() ->
	io:format("test softrealtime begin .....................................~n"),
	N = 42, 
	[ spawn(fun() -> 
				Cost = timer:tc(fun() -> test_fib(UserID, N, fib_iter(N)) end),
				io:format("test round ~p cost:~p~n", [UserID, Cost])
	  end) || UserID <- lists:seq(1, 4)],
	[ begin
		timer:sleep(1000),
		test_high_req()
	  end || _UserID <- lists:seq(1, 10)].


test_high_req() ->
	N = 25, 
	UserID = 200,
	spawn(fun() -> 
				Cost = timer:tc(fun() -> test_fib(UserID, N, fib_iter(N)) end),
				io:format("test high request cost:~p~n", [Cost])
	end).

fib_iter(N) -> fib_iter(N, 0, 1).

fib_iter(0, Pre, _Cur) -> Pre;
fib_iter(1, _Pre, Cur) -> Cur;
fib_iter(N, Pre, Cur) -> fib_iter(N - 1, Cur, Pre + Cur).