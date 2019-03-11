%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% This is the custodian `gen_server' for the tuple space ETS table.
%%%
%%% The table is created as a `set' and in `protected' mode. All
%%% access to the table is expected to come through this server,
%%% although other proceses can inspect the contents for debugging
%%% purposes.
%%%
%%% Each record has the form `{Ref, {Tuple}}', where `Ref' is a unique
%%% key we generate before inserting the record, and `Tuple' is the
%%% user supplied payload. For example if the tuple `{hello, 123}' is
%%% added, then the inserted record will be
%%% `{#Ref<0.3836483324.3974365186.86196>, {hello, 123}}'.
%%%
%%% The ETS table name used will reflect the `espace' instance
%%% name. This will be `tspace' for the default/unnamed instance, and
%%% `tspace_abc' for an instance named `abc'.
%%%
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

%%--------------------------------------------------------------------
%% @doc Add a new tuple to the tuple space ETS table.
%%
%% The tuple is inserted with a unique `reference()' as the key.
%%
%% Once the tuple is added it will trigger the `tspatt_srv' server to
%% check for any waiting (blocking) clients whose `in'/`rd' pattern
%% matches the newly inserted tuple. We do not wait for any replies
%% from `tspatt_srv'.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_tuple(atom(), tuple()) -> done.
add_tuple(Inst_name, Tuple) ->
    gen_server:call(espace:inst_to_name(?SERVER, Inst_name), {add_tuple, Tuple}).

%%--------------------------------------------------------------------
%% @doc Remove the tuple referenced by the supplied unique key.
%%
%% If the record does not exist, it will be ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_tuple(atom(), reference()) -> done.
del_tuple(Inst_name, TabKey) ->
    gen_server:call(espace:inst_to_name(?SERVER, Inst_name), {del_tuple, TabKey}).

%%--------------------------------------------------------------------
%% @doc Lookup a tuple pattern in the tuple space ETS table.
%%
%% If no match is found, `{nomatch}' is returned.
%%
%% If a match is found, `{match, Key, List, Tuple}' is returned, where
%% `Key' uniquely identifies the ETS record, `List' is the list of the
%% `$N' elements referenced in the pattern, if any, and `Tuple' is the
%% second part of the ETS record.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_tuple(atom(), tuple()) -> {nomatch} | {match, {reference(), list(), tuple()}}.
get_tuple(Inst_name, Pattern) ->
    gen_server:call(espace:inst_to_name(?SERVER, Inst_name), {get_tuple, Pattern}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% We expect an instance name to be supplied, which will be used to
%% uniquely identify the ETS table for the instance.
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
-spec handle_call({get_tuple, tuple()} |
		  {add_tuple, tuple()} |
		  {del_tuple, reference()},
		  pid(), term()) ->
			 {reply, {nomatch}, term()} |
			 {reply, {match, {reference(), list(), tuple()}, term()}} |
			 {reply, done, term()}.

handle_call({get_tuple, Pattern}, _From, State) ->
    Reply = handle_get_tuple(State#state.tspool, Pattern),
    {reply, Reply, State};

handle_call({add_tuple, Tuple}, _From, State) ->
    Reply = handle_add_tuple(State#state.inst_name, State#state.tspool, Tuple),
    {reply, Reply, State};

handle_call({del_tuple, TabKey}, _From, State) ->
    Reply = handle_del_tuple(State#state.tspool, TabKey),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(_Msg, State) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc get_tuple handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_get_tuple(ets:tab(), tuple()) -> {nomatch} | {match, tuple()}.
handle_get_tuple(TabId, Pattern) ->
    Match = ets:match(TabId, {'$0', Pattern}, 1),
    case Match of
	'$end_of_table' ->  %% no match
	    {nomatch};
	{[[TabKey|Fields]],_Continuation} -> %% We only want one match, and we ignore the ets:match continuation
	    [{TabKey, Tuple}] = ets:lookup(TabId, TabKey), %% we always also return the whole tuple
	    Reply = {match, {TabKey, Fields, Tuple}}, %% Fields may contain data, if Pattern had '$N'
	    Reply
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc add_tuple handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_tuple(atom(), ets:tid(), tuple()) -> done.
handle_add_tuple(Inst_name, TabId, Tuple) ->
    ets:insert(TabId, {erlang:make_ref(), Tuple}),
    tspatt_srv:check_waitlist(Inst_name, Tuple),
    done.

%%--------------------------------------------------------------------
%% @private
%% @doc del_tuple handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_del_tuple(ets:tab(), reference()) -> done.
handle_del_tuple(TabId, TabKey) ->
    ets:delete(TabId, TabKey),
    done.
