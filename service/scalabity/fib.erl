-module(fib).
-compile(export_all).

%% APIs
calc(N) ->
	fib(N).

%% inner functions
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

