-module(fib_worker).
-compile(export_all).

%% APIs
start() ->
	spawn(fun() -> loop() end).

calc(Pid, N) ->
	rpc(Pid, {calc, N}).
	
%% inner functions
loop() ->
	receive 
		{Ref, From, {calc, N}} ->
			From ! {Ref, {ok, fib:calc(N)}},
			loop()
	end.

rpc(Pid, Req) ->
	Ref = make_ref(),
	Pid ! {Ref, self(), Req},
	receive
		{Ref, Reply} -> Reply
	end.



