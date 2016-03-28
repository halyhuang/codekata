-module(fib_service).
-compile(export_all).

%% APIs
start(N) ->
	pg2:create(service_group),
	[pg2:join(service_group, fib_worker:start()) || _ <- lists:seq(1, N)].

calc(N) ->
	Pid = pg2:get_closest_pid(service_group),
	fib_worker:calc(Pid, N).

calc_cast(N) ->
	Pid = pg2:get_closest_pid(service_group),
	Ref = make_ref(),
	Pid ! {Ref, self(), {calc, N}}.
