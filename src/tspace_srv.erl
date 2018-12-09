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
-export([start_link/1, add_tuple/2, del_tuple/2, get_tuple/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {inst_name, tspool}).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_tuple(atom(), tuple()) -> ok.
add_tuple(Inst_name, Tuple) ->
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {add_tuple, Tuple}).

-spec del_tuple(atom(), reference()) -> ok.
del_tuple(Inst_name, TabKey) ->
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {del_tuple, TabKey}).

-spec get_tuple(atom(), tuple()) -> {nomatch} | {match, {reference(), list(), tuple()}}.
get_tuple(Inst_name, Pattern) ->
    gen_server:call(espace:inst_to_name(?SERVER, Inst_name), {get_tuple, Pattern}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Inst_name) ->
    gen_server:start_link({local, espace:inst_to_name(?SERVER, Inst_name)}, ?MODULE, Inst_name, []).

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
-spec init(atom()) -> {ok, term()}.
init(Inst_name) ->
    process_flag(trap_exit, true),
    Pool_name = espace:inst_to_name(tspace, Inst_name),
    Pool = ets:new(Pool_name, [set, protected]),
    {ok, #state{inst_name=Inst_name, tspool=Pool}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call({get_tuple, term()}, pid(), term()) ->
			 {reply, {nomatch}, term()} | {reply, {match, {reference(), list(), tuple()}, term()}}.
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
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast({add_tuple, tuple()}|{del_tuple, reference()}, term()) -> {noreply, term()}.
handle_cast({add_tuple, Tuple}, State) ->
    ets:insert(State#state.tspool, {erlang:make_ref(), Tuple}),
    tspatt_srv:check_waitlist(State#state.inst_name, Tuple),
    {noreply, State};

handle_cast({del_tuple, TabKey}, State) ->
    ets:delete(State#state.tspool, TabKey),
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
-spec terminate(term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================