-module(op_api).
-compile(export_all).

%% APIs
-spec begin_trans(UserID::term(), N::integer()) -> ok | {error, Reason::term()}.
begin_trans(UserID, N) ->
	rpc({begin_trans, UserID, N}).

-spec op(UserID::term(), Op::atom()) -> ok | {error, Reason::term()}. 
op(UserID, Op) ->
	rpc({op, UserID, Op}).

-spec end_trans(UserID::term()) -> {ok, integer()}.
end_trans(UserID) ->
	rpc({end_trans, UserID}).

rpc(Req) ->
	Ref = make_ref(),
	op_service ! {Ref, self(), Req},
	receive
		{Ref, Reply} ->
			Reply
		after 2000 ->
			{error, timeout}
	end.
