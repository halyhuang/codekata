-module(fib).
-compile(export_all).


calc(N) ->
	fib(N).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

