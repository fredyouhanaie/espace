%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% This is the server that manages the Tuple Space.
%%% @end
%%% Created :  9 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tspool_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, espace_eval/2, espace_out/2, espace_in/2, espace_rd/2, espace_inp/2, espace_rdp/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {inst_name, workersup}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Inst_name) ->
    gen_server:start_link({local, espace:inst_to_name(?SERVER, Inst_name)}, ?MODULE, Inst_name, []).

%%--------------------------------------------------------------------
%% @doc
%% perform an "eval" operation.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_eval(atom(), tuple()) -> 'ok'.
espace_eval(Inst_name, MFA) ->
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {espace_eval, MFA}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation.
%% 
%% @end
%%--------------------------------------------------------------------
-spec espace_out(atom(), tuple()) -> 'ok'.
espace_out(Inst_name, Tuple) ->
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {espace_out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_in(atom(), tuple()) -> any().
espace_in(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_in, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rd(atom(), tuple()) -> any().
espace_rd(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_rd, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "inp" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_inp(atom(), tuple()) -> any().
espace_inp(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_inp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rdp" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rdp(atom(), tuple()) -> any().
espace_rdp(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_rdp, Pattern).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(atom()) -> {'ok',#state{inst_name::atom(),workersup::atom()}}.
init(Inst_name) ->
    process_flag(trap_exit, true),
    {ok, #state{inst_name=Inst_name, workersup=espace:inst_to_name(worker_sup, Inst_name)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(_,_,_) -> {'reply','ok' | {'nomatch'} | {'match',{[any()],_}} | {'nomatch',reference()},_}.
handle_call({espace_in, Pattern}, From, State) ->
    handle_espace_op(espace_in, Pattern, From, State);

handle_call({espace_inp, Pattern}, From, State) ->
    handle_espace_op(espace_inp, Pattern, From, State);

handle_call({espace_rd, Pattern}, From, State) ->
    handle_espace_op(espace_rd, Pattern, From, State);

handle_call({espace_rdp, Pattern}, From, State) ->
    handle_espace_op(espace_rdp, Pattern, From, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

-spec handle_cast(_,_) -> {'noreply',_} | {'stop','normal',_}.
handle_cast({espace_out, Tuple}, State) ->
    tspace_srv:add_tuple(State#state.inst_name, Tuple),
    {noreply, State};

handle_cast(_Msg={espace_eval, {M, F, A}}, State) ->
    supervisor:start_child(State#state.workersup, [M, F, A]),
    {noreply, State};

handle_cast(_Msg={espace_eval, {Fun, Args}}, State) ->
    supervisor:start_child(State#state.workersup, [Fun, Args]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% unified function for "in" and "rd" client API functions
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_op(atom(), 'espace_in' | 'espace_inp' | 'espace_rd' | 'espace_rdp',_) -> any().
espace_op(Inst_name, Espace_Op, Pattern) ->
    Reply = gen_server:call(espace:inst_to_name(?SERVER, Inst_name), {Espace_Op, Pattern}),
    case Reply of
	{match, Match} ->
	    Match;
	{nomatch} -> %% only from the inp and rdp operations
	    nomatch;
	{nomatch, Cli_ref} -> %% only from the in and rd operations
	    receive
		Cli_ref ->
		    espace_op(Inst_name, Espace_Op, Pattern) % our tuple has arrived, try again!
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% unified call handler for "in", "inp", "rd" and "rdp"
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_espace_op('espace_in' | 'espace_inp' | 'espace_rd' | 'espace_rdp',_,_,_) -> {'reply',{'nomatch'} | {'match',{[any()],_}} | {'nomatch',reference()},any()}.
handle_espace_op(Espace_Op, Pattern, From, State) ->
    case tspace_srv:get_tuple(State#state.inst_name, Pattern) of
	{nomatch} ->
	    case Espace_Op of
		espace_inp ->
		    {reply, {nomatch}, State};
		espace_rdp ->
		    {reply, {nomatch}, State};
		_ -> %% only "in" and "rd" should block on no match
		    {Cli_pid, _} = From,  %% we use the pid to notify the client
		    Cli_ref = make_ref(), %% the client should wait for this ref
		    tspatt_srv:add_pattern(State#state.inst_name, Cli_ref, Pattern, Cli_pid),
		    {reply, {nomatch, Cli_ref}, State}
	    end;
	{match, {TabKey, Fields, Tuple}} ->
	    Reply = {match, {Fields, Tuple}},
	    case Espace_Op of   %% "in" and "inp" should remove the tuple
		espace_in ->
		    tspace_srv:del_tuple(State#state.inst_name, TabKey);
		espace_inp ->
		    tspace_srv:del_tuple(State#state.inst_name, TabKey);
		_ ->
		    ok
	    end,
	    {reply, Reply, State}
    end.
