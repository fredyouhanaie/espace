%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tspool_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, espace_out/1, espace_in/1, espace_rd/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tspool, tspatt}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation.
%% 
%% @spec espace_out(Tuple) -> ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

espace_out(Tuple) ->
    gen_server:cast(?SERVER, {espace_out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation
%% @spec espace_in(Pattern) -> [any()] | {error, Error}
%% @end
%%--------------------------------------------------------------------
espace_in(Pattern) ->
    espace_op(espace_in, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation
%% @spec espace_in(Pattern) -> [any()] | {error, Error}
%% @end
%%--------------------------------------------------------------------
espace_rd(Pattern) ->
    espace_op(espace_rd, Pattern).


%%--------------------------------------------------------------------
%% @doc
%% stop the server.
%% 
%% @spec stop() -> ignore
%% @end
%%--------------------------------------------------------------------

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% We create two ETS tables, the main tuple space pool, and one for
%% patterns that do not yet exist in the TS pool.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Pool = ets:new(tspace, [duplicate_bag, protected]),
    Patt = ets:new(tspace_patt, [duplicate_bag, protected]),
    {ok, #state{tspool=Pool, tspatt=Patt}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({espace_in, Pattern}, From, State) ->
    handle_espace_op(espace_in, Pattern, From, State);

handle_call({espace_rd, Pattern}, From, State) ->
    handle_espace_op(espace_rd, Pattern, From, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({espace_out, Tuple}, State) ->
    ets:insert(State#state.tspool, {erlang:make_ref(), Tuple}),
    TSpatt = State#state.tspatt,
    check_waitlist(Tuple, TSpatt, ets:tab2list(TSpatt)), % this will tell any waiting in/rd to recurse!
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_waitlist(_Tuple, _TabId, []) ->
    none;
check_waitlist(Tuple, TabId, [Cli|Clients]) ->
    {Pattern, Cli_pid, Cli_ref} = Cli,
    case ets:test_ms(Tuple, [{Pattern,[],['$$']}]) of
	{ok, false} ->
	    check_waitlist(Tuple, TabId, Clients);
	_ ->
	    Cli_pid ! Cli_ref, %% don't forget to delete the tspatt!
	    ets:delete_object(TabId, Cli)
    end.

%%--------------------------------------------------------------------
%% @doc
%% unified function for "in" and "rd" client API functions
%% @spec
%% @end
%%--------------------------------------------------------------------
espace_op(Espace_Op, Pattern) ->
    Reply = gen_server:call(?SERVER, {Espace_Op, Pattern}),
    case Reply of
	{match, Match} ->
	    Match;
	{nomatch, Cli_ref} ->
	    receive
		Cli_ref ->
		    Espace_Op(Pattern) % our tuple has arrived, try again!
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% unified call handler for "in" and "rd"
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_espace_op(Espace_Op, Pattern, From, State) ->
    TabId = State#state.tspool,
    Match = ets:match(TabId, {'$0', Pattern}, 1),
    case Match of
	'$end_of_table' ->
	    {Cli_pid, _} = From,  %% so that we can notify the client
	    Cli_ref = make_ref(), %% so that we have a unique id to send to the client
	    ets:insert(State#state.tspatt, {Pattern, Cli_pid, Cli_ref}),
	    {reply, {nomatch, Cli_ref}, State};
	{[[TabKey|Fields]],_} -> %% We only want one match, and we ignore the ets:match continuation
	    [{TabKey, Tuple}] = ets:lookup(TabId, TabKey), %% we always return the whole tuple
	    Reply = {match, {Fields, Tuple}}, %% Fileds may contain data, if Pattern had '$N'
	    case Espace_Op of
		espace_in ->
		    ets:delete(TabId, TabKey);
		_ ->
		    ok
	    end,
	    {reply, Reply, State}
    end.
