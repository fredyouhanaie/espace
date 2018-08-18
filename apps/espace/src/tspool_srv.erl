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
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation.
%% 
%% @end
%%--------------------------------------------------------------------
-spec espace_out(_) -> 'ok'.
espace_out(Tuple) ->
    gen_server:cast(?SERVER, {espace_out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_in(_) -> any().
espace_in(Pattern) ->
    espace_op(espace_in, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rd(_) -> any().
espace_rd(Pattern) ->
    espace_op(espace_rd, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "inp" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_inp(_) -> any().
espace_inp(Pattern) ->
    espace_op(espace_inp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rdp" operation
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rdp(_) -> any().
espace_rdp(Pattern) ->
    espace_op(espace_rdp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server.
%% 
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
    tspace_srv:add_tuple(Tuple),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
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
%%
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
%%
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
