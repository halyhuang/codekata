-module(test).
-compile(export_all).

gen_client() ->
	spawn(fun() ->
			io:format("~p~n", [timer:tc(fun() -> fib_service:calc(40),ok end)])
		  end).

test_client() ->
	fib_service:start(),
	[gen_client() || _ <- lists:seq(1, 10)],
	ok.







