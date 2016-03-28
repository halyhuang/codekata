-module(op_service).
-compile(export_all).
-record(user_info, {uid, num, pending_ops=[], receiver = none}).
-record(worker_info, {pid, uid, op, num}).
-record(state, {users = [], workers = []}).

%% APIs
start() ->
	LeaderPid = op_api:get_leader(),
	net_adm:ping(node(LeaderPid)),
	Pid = spawn(?MODULE, loop, [#state{}]),
	register(?MODULE, Pid),	
	{ok, Pid}.

%% inner functions
loop(State) ->
	receive 
		{op_result, UserID, Result} ->
			NewState = handle_op_result(UserID, Result, State),
			loop(NewState);
		{'DOWN', _MonitorRef, process, Pid, _Reason} ->
			NewState = handle_monitor_down(Pid, State),
			loop(NewState);
		{Ref, From, Message} ->
			NewState = handle_message(Message, {Ref, From}, State),
			loop(NewState)
	end.


handle_message({begin_trans, UserID, N}, {Ref, From}, #state{users=Users} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			NewUsers = [#user_info{uid = UserID, num = N} | Users],
			From ! {Ref, ok},
			#state{users = NewUsers, workers = State#state.workers};
		#user_info{} ->
			From ! {Ref, {error, duplicated_trans}},
			State
	end;
handle_message({op, UserID, Op}, {Ref, From}, #state{users = Users, workers = Workers} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			From ! {Ref, {error, user_not_found}},
			State;	
		#user_info{receiver = Receiver} when Receiver =/= none ->
			From ! {Ref, {error, trans_ended}},
			State;
		#user_info{num = N, pending_ops = []} = Usr ->
			From ! {Ref, ok},
			{ok, Pid} = op_worker:start_monitor(self(), UserID, Op, N),
			NewWorkers = [ #worker_info{pid = Pid, uid = UserID, op = Op, num = N} | Workers],
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = [Op]}),
			#state{users = NewUsers, workers = NewWorkers};
		#user_info{pending_ops = POPs} = Usr ->
			From ! {Ref, ok},
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = POPs ++ [Op]}),
			State#state{users = NewUsers}
	end;
handle_message({end_trans, UserID}, {Ref, From}, #state{users = Users} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			From ! {Ref, {error, user_not_found}},
			State;	
		#user_info{num = N, pending_ops = []} ->
			From ! {Ref, {ok, N}},
			NewUsers = lists:keydelete(UserID, #user_info.uid, Users),			
			State#state{users = NewUsers};
		#user_info{} = Usr ->
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{receiver = {Ref, From}}),
			State#state{users = NewUsers}
	end.				

handle_op_result(UserID, Result, #state{users = Users, workers = Workers} = State) ->
	case {lists:keyfind(UserID, #user_info.uid, Users),
			lists:keyfind(UserID, #worker_info.uid, Workers)} of
		{#user_info{pending_ops = [_Op1, Op2 | T]} = Usr, #worker_info{}} ->
			{ok, Pid} = op_worker:start_monitor(self(), UserID, Op2, Result),
			NewWorkers = lists:keydelete(UserID, #worker_info.uid, Workers),
			NewWorkers2 = [ #worker_info{pid = Pid, uid = UserID, op = Op2, num = Result} | NewWorkers],
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = [Op2 | T], num = Result}),			
			State#state{users = NewUsers, workers = NewWorkers2};
		{#user_info{pending_ops = [_Op1]} = Usr, #worker_info{}} ->
			NewWorkers = lists:keydelete(UserID, #worker_info.uid, Workers),			
			NewUsers = 
				case Usr#user_info.receiver of
					none ->
						lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{num = Result, pending_ops = []});
					{Ref, Receiver} ->
						Receiver ! {Ref, {ok, Result}},
						lists:keydelete(UserID, #user_info.uid, Users)
				end,
			State#state{users = NewUsers, workers = NewWorkers};
		_ ->
			io:format("unexpected ~p ~n", [UserID]),
			State
	end.

handle_monitor_down(WorkerPid, #state{workers = Workers} = State) ->
	case lists:keyfind(WorkerPid, #worker_info.pid, Workers) of
		#worker_info{uid = UserID, op = Op, num = N} = Worker ->
			{ok, Pid} = op_worker:start_monitor(self(), UserID, Op, N),
			NewWorkers = lists:keyreplace(UserID, #worker_info.uid, Workers, 
									Worker#worker_info{pid = Pid}),
			State#state{workers = NewWorkers};
		false ->
			State
	end.
	
