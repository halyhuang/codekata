-module(op_api).
-compile(export_all).

%% APIs
-spec begin_trans(UserID::term(), N::integer(), Timeout::integer()) -> ok | {error, Reason::term()}.
begin_trans(UserID, N, Timeout) ->
	rpc({begin_trans, UserID, N}, Timeout).

begin_trans(UserID, N) -> 
	begin_trans(UserID, N, 2000).

-spec op(UserID::term(), Op::atom(), Timeout::integer()) -> ok | {error, Reason::term()}. 
op(UserID, Op, Timeout) ->
	rpc({op, UserID, Op}, Timeout).

op(UserID, Op) ->	
	op(UserID, Op, 2000).

-spec end_trans(UserID::term(), Timeout::integer()) -> {ok, integer()}.
end_trans(UserID, Timeout) ->
	rpc({end_trans, UserID}, Timeout).

end_trans(UserID) ->
	end_trans(UserID, 2000).

rpc(Req, Timeout) ->
	Ref = make_ref(),
	get_leader() ! {Ref, self(), Req},
	receive
		{Ref, Reply} ->
			Reply
		after Timeout ->
			{error, timeout}
	end.

get_leader() ->
	case global:whereis_name(op_service) of 
		undefined ->
			timer:sleep(random:uniform(500)),
			get_leader();
		LeaderPid ->
			LeaderPid
	end.