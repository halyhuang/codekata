-module(op_service).
-compile(export_all).
-record(user_info, {uid, num, pending_ops=[], receiver = none}).
-record(worker_info, {pid, uid, op, num}).
-record(state, {users = [], workers = [], leader = undefined, followers = [], nodes = []}).

%% APIs
start() ->
	Pid = spawn(?MODULE, init, [#state{}]),
	register(?MODULE, Pid),	
	{ok, Pid}.

%% inner functions
init(State) ->
	{Role, NewState} = try_leader(State#state{leader = self()}),
	play_role({Role, NewState}).

report_membership(LeaderPid, FollowerPid) ->
	LeaderPid ! {report_membership, FollowerPid}.

try_leader(State = #state{workers = Workers}) ->
		Pid = register_leader(State#state.leader),
		NewState = State#state{leader = Pid, followers = [], nodes = [node(Pid)]},
		if
			Pid =:= self() ->
				io:format("I am leader ~p on Node ~p~n", [Pid, node(Pid)]),
				[erlang:monitor(process, WPid) || #worker_info{pid = WPid} <- Workers],
				{leader, NewState};
			true ->
				erlang:monitor(process, Pid),
				io:format("I am follower ~p on Node ~p~n", [self(), node()]),
				report_membership(Pid, self()),
				{follower, NewState}
		end.

register_leader(OldLeaderPid) ->
	case global:whereis_name(?MODULE) of 
		undefined ->
			case global:register_name(?MODULE, self()) of
				yes ->
					self();
				no ->
					global:whereis_name(?MODULE)
			end;
		OldLeaderPid ->
			timer:sleep(random:uniform(500)),
			register_leader(OldLeaderPid);
		NewLeaderPid ->
			NewLeaderPid
	end.

play_role({leader, State}) ->
	loop_leader(State);
play_role({follower, State}) ->
	loop_follower(State).

loop_leader(State) ->
	receive 
		{op_result, UserID, Result} ->
			NewState = handle_op_result(UserID, Result, State),
			loop_leader(NewState);
		{'DOWN', _MonitorRef, process, Pid, _Reason} ->
			NewState = handle_monitor_down(Pid, State),
			loop_leader(NewState);
		{report_membership, FollowerPid} ->
			NewState = handle_membership_report(FollowerPid, State),
			loop_leader(NewState);
		{Ref, From, Message} ->
			NewState = handle_message(Message, {Ref, From}, State),
			loop_leader(NewState)
	end.

loop_follower(State=#state{leader = LeaderPid}) ->
	receive
		{'DOWN', _MonitorRef, process, LeaderPid, _Reason} ->
			{Role, NewState} = try_leader(State),
			play_role({Role, NewState});
		{sync_info_from_leader, Ref, From, {Label, Info}} ->
			NewState = handle_sync_message(Label, Info, State),
			From ! {Ref, ok},
			loop_follower(NewState)
	end.

handle_sync_message(Label, Info, State) ->
	case {Label, Info} of
		{sync_state_from_leader, NewState} ->
			NewState;
		{followers_and_nodes, {NewFollowers, NewNodes}} ->
			State#state{followers = NewFollowers, nodes = NewNodes};
		{workers, NewWorkers} ->
			State#state{workers = NewWorkers};
		{users, NewUsers} ->
			State#state{users = NewUsers};
		{workers_and_users, {NewWorkers, NewUsers}} ->
			State#state{users = NewUsers, workers = NewWorkers}
	end.

handle_membership_report(FollowerPid, State) ->
	monitor(process, FollowerPid),
	NewFollowers = State#state.followers ++ [FollowerPid],
	NewNodes = State#state.nodes ++ [node(FollowerPid)],
	NewState = State#state{followers = NewFollowers, nodes = NewNodes},
	sync([FollowerPid], sync_state_from_leader, NewState),
	sync(State#state.followers, followers_and_nodes, {NewFollowers, NewNodes}),
	io:format("A new follower ~p on Node ~p~n", [FollowerPid, node(FollowerPid)]),	
	NewState.

sync(Followers, Label, Info) ->
	[ begin 
		Ref = make_ref(),
		FollowerPid ! {sync_info_from_leader, Ref, self(), {Label, Info}},
		receive 
			{Ref, Reply} ->
				Reply
			after 2000 ->
				io:format("Sync follower ~p {label:~p, info:~p }failed!~n", [node(FollowerPid), Label, Info]),
				{error, timeout}
		end
	  end	|| FollowerPid <- Followers].	

schedule_node([Node | T]) ->
	io:format("worker node ~p~n", [Node]),
	{Node, T ++ [Node]}.

handle_message({begin_trans, UserID, N}, {Ref, From}, #state{users=Users} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			NewUsers = [#user_info{uid = UserID, num = N} | Users],
			sync(State#state.followers, users, NewUsers),
			From ! {Ref, ok},
			State#state{users = NewUsers};
		#user_info{} ->
			From ! {Ref, {error, duplicated_trans}},
			State
	end;
handle_message({op, UserID, Op}, {Ref, From}, #state{users = Users, workers = Workers, nodes = Nodes} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			From ! {Ref, {error, user_not_found}},
			State;	
		#user_info{receiver = Receiver} when Receiver =/= none ->
			From ! {Ref, {error, trans_ended}},
			State;
		#user_info{num = N, pending_ops = []} = Usr ->
			{Node, NewNodes} = schedule_node(Nodes),
			{ok, Pid} = op_worker:start_monitor(Node, self(), UserID, Op, N),
			NewWorkers = [ #worker_info{pid = Pid, uid = UserID, op = Op, num = N} | Workers],
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = [Op]}),
			sync(State#state.followers, workers_and_users, {NewWorkers, NewUsers}),
			From ! {Ref, ok},
			State#state{users = NewUsers, workers = NewWorkers, nodes = NewNodes};
		#user_info{pending_ops = POPs} = Usr ->
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = POPs ++ [Op]}),
			sync(State#state.followers, users, NewUsers),						
			From ! {Ref, ok},
			State#state{users = NewUsers}
	end;
handle_message({end_trans, UserID}, {Ref, From}, #state{users = Users} = State) ->
	case lists:keyfind(UserID, #user_info.uid, Users) of
		false ->
			From ! {Ref, {error, user_not_found}},
			State;	
		#user_info{num = N, pending_ops = []} ->
			NewUsers = lists:keydelete(UserID, #user_info.uid, Users),			
			sync(State#state.followers, users, NewUsers),						
			From ! {Ref, {ok, N}},
			State#state{users = NewUsers};
		#user_info{} = Usr ->
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{receiver = {Ref, From}}),
			sync(State#state.followers, users, NewUsers),
			State#state{users = NewUsers}
	end.				

handle_op_result(UserID, Result, #state{users = Users, workers = Workers, nodes = Nodes} = State) ->
	case {lists:keyfind(UserID, #user_info.uid, Users),
			lists:keyfind(UserID, #worker_info.uid, Workers)} of
		{#user_info{pending_ops = [_Op1, Op2 | T]} = Usr, #worker_info{}} ->
			{Node, NewNodes} = schedule_node(Nodes),
			{ok, Pid} = op_worker:start_monitor(Node, self(), UserID, Op2, Result),
			NewWorkers = lists:keydelete(UserID, #worker_info.uid, Workers),
			NewWorkers2 = [ #worker_info{pid = Pid, uid = UserID, op = Op2, num = Result} | NewWorkers],
			NewUsers = lists:keyreplace(UserID, #user_info.uid, Users, Usr#user_info{pending_ops = [Op2 | T], num = Result}),
			sync(State#state.followers, workers_and_users, {NewWorkers2, NewUsers}),
			State#state{users = NewUsers, workers = NewWorkers2, nodes = NewNodes};
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
		    sync(State#state.followers, workers_and_users, {NewWorkers, NewUsers}),				
			State#state{users = NewUsers, workers = NewWorkers};
		_ ->
			io:format("unexpected ~p ~n", [UserID]),
			State
	end.

handle_monitor_down(DownPid, #state{workers = Workers, nodes = Nodes} = State) ->
	case lists:member(DownPid, State#state.followers) of
		true ->
			NewFollowers = State#state.followers -- [DownPid],
			NewNodes = State#state.nodes -- [node(DownPid)],
			sync(NewFollowers, followers_and_nodes, {NewFollowers, NewNodes}),
			State#state{followers = NewFollowers, nodes = NewNodes};
		false ->
			case lists:keyfind(DownPid, #worker_info.pid, Workers) of
				#worker_info{uid = UserID, op = Op, num = N} = Worker ->
					{Node, NewNodes} = schedule_node(Nodes),
					{ok, Pid} = op_worker:start_monitor(Node, self(), UserID, Op, N),
					NewWorkers = lists:keyreplace(UserID, #worker_info.uid, Workers, 
											Worker#worker_info{pid = Pid}),
				    sync(State#state.followers, workers, NewWorkers),
					State#state{workers = NewWorkers, nodes = NewNodes};
				false ->
					State
			end
	end.
