-module(fib).
-compile(export_all).

%% APIs
calc(N) when is_integer(N) and (N >= 0)->
	fib(N).

%% inner functions
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

