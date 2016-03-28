-module(fib_worker).
-compile(export_all).

start(Ref, From, {calc, N}) ->
	From ! {Ref, {ok, fib:calc(N)}}.

