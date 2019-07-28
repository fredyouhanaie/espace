% -*- indent-tabs-mode:nil; -*-
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
%%% name. This will be `espace_tspace' for the default/unnamed
%%% instance, and `espace_tspace_abc' for an instance named `abc'.
%%%
%%% The `etsmgr' application is used to add resiliency to the server
%%% data, should the server restart while it is holding tuple space
%%% data.
%%%
%%% @end
%%% Created : 10 Jan 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_tspace_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, add_tuple/2, del_tuple/2, get_tuple/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([handle_continue/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, espace_tspace).
-define(TABLE_OPTS, [set, protected]).

-record(state, {inst_name, tspace_tabid, etsmgr_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Add a new tuple to the tuple space ETS table.
%%
%% The tuple is inserted with a unique `reference()' as the key.
%%
%% Once the tuple is added it will trigger the `espace_tspatt_srv'
%% server to check for any waiting (blocking) clients whose `in'/`rd'
%% pattern matches the newly inserted tuple. We do not wait for any
%% replies from `espace_tspatt_srv'.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_tuple(atom(), tuple()) -> done.
add_tuple(Inst_name, Tuple) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {add_tuple, Tuple}).

%%--------------------------------------------------------------------
%% @doc Remove the tuple referenced by the supplied unique key.
%%
%% If the record does not exist, it will be ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec del_tuple(atom(), reference()) -> done.
del_tuple(Inst_name, TabKey) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {del_tuple, TabKey}).

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
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {get_tuple, Pattern}).


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
    gen_server:start_link({local, espace_util:inst_to_name(?SERVER, Inst_name)}, ?MODULE, Inst_name, []).

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
-spec init(atom()) -> {ok, term(), term()}.
init(Inst_name) ->
    process_flag(trap_exit, true),
    {ok, #state{inst_name=Inst_name}, {continue, init}}.


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
    Reply = handle_get_tuple(State#state.tspace_tabid, Pattern),
    {reply, Reply, State};

handle_call({add_tuple, Tuple}, _From, State) ->
    Reply = handle_add_tuple(State#state.inst_name, State#state.tspace_tabid, Tuple),
    {reply, Reply, State};

handle_call({del_tuple, TabKey}, _From, State) ->
    Reply = handle_del_tuple(State#state.tspace_tabid, TabKey),
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
%% Handling continue requests.
%%
%% We use `{continue, init}' from `espace_tspace_srv:init/1' to ensure that
%% `etsmgr' is started and is managing our ETS table before handling
%% the first request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(term(), term()) -> {noreply, term()} | {stop, term()}.
handle_continue(init, State) ->
    case handle_wait4etsmgr(init, State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Error} ->
            {stop, Error}
    end;

handle_continue(_Continue, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% We can expect an `EXIT' message if the `etsmgr' server exits
%% unexpectedly. In this case we need to wait for the new `etsmgr'
%% server to restart and be told of our ETS table before continuing
%% further.
%%
%% We can also expect `ETS-TRANSFER' messages whenever we are handed
%% over the ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()} |
                         {noreply, NewState :: term(), Timeout :: timeout()} |
                         {noreply, NewState :: term(), hibernate} |
                         {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'EXIT', Pid, _Reason}, State) ->
    case State#state.etsmgr_pid of
        Pid ->
            case handle_wait4etsmgr(recover, State) of
                {ok, State2} ->
                    {noreply, State2};
                {error, Error} ->
                    {stop, Error}
            end;
        _Other_pid ->
            {noreply, State}
    end;

handle_info({'ETS-TRANSFER', _Table_id, _From_pid, _Gift_data}, State) ->
    {noreply, State};

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
-spec terminate(term(), term()) -> ok.
terminate(_Reason, State) ->
    Inst_name = State#state.inst_name,
    Table_name = espace_util:inst_to_name(?TABLE_NAME, Inst_name),
    etsmgr:del_table(Inst_name, Table_name),
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
    espace_tspatt_srv:check_waitlist(Inst_name, Tuple),
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

%%--------------------------------------------------------------------
%% @doc wait for etsmgr to (re)start, ensure it manages our ETS table,
%% and update the `State' data.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_wait4etsmgr(atom(), term()) -> {ok, term()} | {error, term()}.
handle_wait4etsmgr(Mode, State) ->
    Inst_name = State#state.inst_name,
    Table_name = espace_util:inst_to_name(?TABLE_NAME, Inst_name),

    Result = case Mode of
                 init ->
                     espace_util:wait4etsmgr(Inst_name, init, Table_name, ?TABLE_OPTS);
                 recover ->
                     espace_util:wait4etsmgr(Inst_name, recover, Table_name, State#state.tspace_tabid)
             end,

    case Result of
        {ok, Mgr_pid, Table_id} ->
            {ok, State#state{etsmgr_pid=Mgr_pid, tspace_tabid=Table_id}};
        {error, Error} ->
            {error, Error}
    end.
