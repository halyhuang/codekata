-module(client).
-compile(export_all).

test() ->
	io:format("test begin .....................................~n"),
	test(0).

test(N) ->
	op_api:begin_trans(1, 4),
	op_api:op(1, double),
	op_api:op(1, fib),
	{ok, 21} = op_api:end_trans(1),
	io:format("test round ~p ok.~n", [N]),
	timer:sleep(100),
	test(N + 1).
