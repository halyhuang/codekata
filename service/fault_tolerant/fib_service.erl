-module(fib_service).
-compile(export_all).

start() ->
	Pid = spawn(fun() -> loop() end),
	register(?MODULE, Pid),
	{ok, Pid}.

calc(N) ->
	Ref = make_ref(),
	?MODULE ! {Ref, self(), {calc, N}},
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
