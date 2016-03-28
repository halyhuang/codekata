-module(fib_service).
-compile(export_all).

start() ->
	Pid = spawn(fun() -> loop() end),
	pg2:create(service_group),	
	pg2:join(service_group, Pid),
	{ok, Pid}.

calc(N) ->
	Ref = make_ref(),
	Pid = pg2:get_closest_pid(service_group),
	Pid ! {Ref, self(), {calc, N}},
	receive 
		{Ref, Reply} -> Reply
		after 2000 -> {error, timeout}
	end.

loop() ->
	receive 
		{Ref, From, Req} ->
			spawn(fun() -> fib_worker:start(Ref, From, Req) end),
			loop()
	end.
