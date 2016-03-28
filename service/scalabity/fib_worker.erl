-module(fib_worker).
-compile(export_all).

%% APIs
start() ->
	spawn(fun() -> loop() end).

%% inner functions
loop() ->
	receive 
		{Ref, From, {calc, N}} ->
			From ! {Ref, {ok, fib:calc(N)}},
			loop()
	end.


