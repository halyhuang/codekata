-module(fib_service).
-compile(export_all).

%% APIs
start(N) ->
	spawn(fun() -> init(N) end).

calc(Pid, N) ->
	rpc(Pid, {calc, N}).

calc_cast(Pid, N) ->
	Ref = make_ref(),
	Pid ! {Ref, self(), {calc, N}}.

%% inner functions

init(N) ->	
	Workers = [ fib_worker:start() || _ <- lists:seq(1, N)],
	loop(Workers).

get_worker([ H | T]) ->
	{H, T ++ [H]}.

loop(Workers) ->
	receive 
		{Ref, From, Req} ->
			{Worker, NewWorkers} = get_worker(Workers),
			Worker ! {Ref, From, Req},
			loop(NewWorkers)
	end.


rpc(Pid, Req) ->
	Ref = make_ref(),
	Pid ! {Ref, self(), Req},
	receive
		{Ref, Reply} -> Reply
	end.


