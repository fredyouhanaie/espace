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
-export([start_link/0, espace_out/1, espace_in/1, stop/0]).

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
    Match = gen_server:call(?SERVER, {espace_in, Pattern}),
    io:format("espace_in: ~p.~n", [Match]),
    Match.

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

handle_call({espace_in, Pattern}, _From, State) ->
    Match = ets:match(State#state.tspool, {'_', Pattern}),
    case Match of
	[] ->
	    {reply, notfound, State};
	_ ->
	    {reply, {match, Match}, State}
    end;

handle_call({espace_inp, _Pattern}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({espace_rd, _Pattern}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({espace_rdp, _Pattern}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

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
