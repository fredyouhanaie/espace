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
-export([start_link/0, espace_out/1, espace_in/1, espace_rd/1, espace_inp/1, espace_rdp/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation.
%% 
%% @spec espace_out(Tuple) -> ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec espace_out(_) -> 'ok'.
espace_out(Tuple) ->
    gen_server:cast(?SERVER, {espace_out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation
%% @spec espace_in(Pattern) -> [any()] | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec espace_in(_) -> any().
espace_in(Pattern) ->
    espace_op(espace_in, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation
%% @spec espace_rd(Pattern) -> {list() | tuple()}
%% @end
%%--------------------------------------------------------------------
-spec espace_rd(_) -> any().
espace_rd(Pattern) ->
    espace_op(espace_rd, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "inp" operation
%% @spec espace_inp(Pattern) -> {list() | tuple()}
%% @end
%%--------------------------------------------------------------------
-spec espace_inp(_) -> any().
espace_inp(Pattern) ->
    espace_op(espace_inp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rdp" operation
%% @spec espace_rdp(Pattern) -> {list() | tuple()}
%% @end
%%--------------------------------------------------------------------
-spec espace_rdp(_) -> any().
espace_rdp(Pattern) ->
    espace_op(espace_rdp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server.
%% 
%% @spec stop() -> ignore
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
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
-spec init([]) -> {'ok', {}}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, {}}.

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
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

-spec handle_cast(_,_) -> {'noreply',_} | {'stop','normal',_}.
handle_cast({espace_out, Tuple}, State) ->
    tspace_srv:add_tuple(Tuple),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.


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
-spec handle_info(_,_) -> {'noreply',_}.
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
-spec terminate(_,_) -> 'ok'.
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
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% unified function for "in" and "rd" client API functions
%% @spec espace_op(atom(), tuple()) -> nomatch | {list(), tuple()}
%% @end
%%--------------------------------------------------------------------
-spec espace_op('espace_in' | 'espace_inp' | 'espace_rd' | 'espace_rdp',_) -> any().
espace_op(Espace_Op, Pattern) ->
    Reply = gen_server:call(?SERVER, {Espace_Op, Pattern}),
    case Reply of
	{match, Match} ->
	    Match;
	{nomatch} -> %% only from the inp and rdp operations
	    nomatch;
	{nomatch, Cli_ref} -> %% only from the in and rd operations
	    receive
		Cli_ref ->
		    espace_op(Espace_Op, Pattern) % our tuple has arrived, try again!
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% unified call handler for "in", "inp", "rd" and "rdp"
%% @spec handle_espace_op(atom(), tuple(), tuple(), tuple()) -> {atom(), tuple(), tuple()}
%% @end
%%--------------------------------------------------------------------
-spec handle_espace_op('espace_in' | 'espace_inp' | 'espace_rd' | 'espace_rdp',_,_,_) -> {'reply',{'nomatch'} | {'match',{[any()],_}} | {'nomatch',reference()},any()}.
handle_espace_op(Espace_Op, Pattern, From, State) ->
    case tspace_srv:get_tuple(Pattern) of
	{nomatch} ->
	    case Espace_Op of
		espace_inp ->
		    {reply, {nomatch}, State};
		espace_rdp ->
		    {reply, {nomatch}, State};
		_ -> %% only "in" and "rd" should block on no match
		    {Cli_pid, _} = From,  %% we use the pid to notify the client
		    Cli_ref = make_ref(), %% the client should wait for this ref
		    tspatt_srv:add_pattern(Cli_ref, Pattern, Cli_pid),
		    {reply, {nomatch, Cli_ref}, State}
	    end;
	{match, {TabKey, Fields, Tuple}} ->
	    Reply = {match, {Fields, Tuple}},
	    case Espace_Op of   %% "in" and "inp" should remove the tuple
		espace_in ->
		    tspace_srv:del_tuple(TabKey);
		espace_inp ->
		    tspace_srv:del_tuple(TabKey);
		_ ->
		    ok
	    end,
	    {reply, Reply, State}
    end.
