-module(test).
-compile(export_all).

gen_client(Pid) ->
	spawn(fun() ->
			io:format("~p~n", [timer:tc(fun() -> fib_service:calc(Pid, 35),ok end)])
		  end).

test_client(Workers) ->
	Pid = fib_service:start(Workers),
	[gen_client(Pid) || _ <- lists:seq(1, 10)],
	ok.







