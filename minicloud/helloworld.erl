-module(helloworld).
-compile(export_all).

loop() ->
	receive 
		{From, {who}} -> From ! {who, "Server"}, loop();
		{_From, {quit}} -> io:format("Server quit!~n");
		{From, Word} -> 
			From ! Word, 
			loop();
		All ->
			io:format("Receive ~p~n", [All]), loop()
	end.


handle_call(Pid, Word) ->
	Pid ! {self(), Word},
	receive 
		Word -> Word
	end.


handle_cast(Pid, Word) ->
	Pid ! {self(), Word}.

test_say_hello() ->
	Pid = spawn(fun() -> loop() end),
	handle_cast(Pid, {who}),
	receive 
		{who, Name} -> io:format("I am ~p~n", [Name])		
	end,
	{word, "hi"} = handle_call(Pid, {word, "hi"}),
	handle_cast(Pid, {quit}).

