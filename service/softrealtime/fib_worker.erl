-module(fib_worker).
-compile(export_all).

start(Ref, From, {calc, N}) ->
	From ! {Ref, {ok, fib(N)}}.

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N -1) + fib(N - 2).