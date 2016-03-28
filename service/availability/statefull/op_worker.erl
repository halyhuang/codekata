-module(op_worker).
-compile(export_all).

start_monitor(Owner, UserID, Op, N) ->
	random:seed(now()),
	N2 = case random:uniform(5) > 2 of
			true -> N;
			false -> not_a_num
		  end,
	Pid = spawn(fun() -> 
				Owner ! {op_result, UserID, op(Op, N2)} end),
	monitor(process, Pid),
	{ok, Pid}.
	
op(fib, N) -> fib:calc(N);
op(double, N) -> N * 2.	




