%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% This is the custodian server for the tuple space ETS table
%%% @end
%%% Created : 10 Jan 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tspace_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, add_tuple/1, del_tuple/1, get_tuple/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tspool}).

%%%===================================================================
%%% API
%%%===================================================================

add_tuple(Tuple) ->
    gen_server:cast(?SERVER, {add_tuple, Tuple}).

del_tuple(Tuple) ->
    gen_server:cast(?SERVER, {del_tuple, Tuple}).

get_tuple(Pattern) ->
    gen_server:call(?SERVER, {get_tuple, Pattern}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    Pool = ets:new(tspace, [set, protected]),
    {ok, #state{tspool=Pool}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get_tuple, Pattern}, _From, State) ->
    TabId = State#state.tspool,
    Match = ets:match(TabId, {'$0', Pattern}, 1),
    case Match of
	'$end_of_table' ->  %% no match
	    {reply, {nomatch}, State};
	{[[TabKey|Fields]],_Continuation} -> %% We only want one match, and we ignore the ets:match continuation
	    [{TabKey, Tuple}] = ets:lookup(TabId, TabKey), %% we always also return the whole tuple
	    Reply = {match, {TabKey, Fields, Tuple}}, %% Fields may contain data, if Pattern had '$N'
	    {reply, Reply, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({add_tuple, Tuple}, State) ->
    ets:insert(State#state.tspool, {erlang:make_ref(), Tuple}),
    tspatt_srv:check_waitlist(Tuple),
    {noreply, State};

handle_cast({del_tuple, TabKey}, State) ->
    ets:delete(State#state.tspool, TabKey),
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
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
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
