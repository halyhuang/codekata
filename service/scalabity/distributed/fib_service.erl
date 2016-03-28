-module(fib_service).
-compile(export_all).

%% APIs
start() ->
	pg2:create(fib_group),
	[pg2:join(fib_group, fib_worker:start(Node)) || Node <- nodes()].

calc(N) ->
	Pid = pg2:get_closest_pid(fib_group),
	fib_worker:calc(Pid, N).
		