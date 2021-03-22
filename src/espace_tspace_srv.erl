%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% This is the custodian `gen_server' for the tuple space ETS table.
%%%
%%% The table is created as an `ordered_set' and in `protected'
%%% mode. All access to the table is expected to come through this
%%% server, although other proceses can inspect the contents for
%%% debugging purposes.
%%%
%%% Each record has the form `{Num, {Tuple}}', where `Num' is a unique
%%% integer key that we initialized to 1 and increment after inserting
%%% the record, and `Tuple' is the user supplied payload. For example
%%% if the tuple `{hello, 123}' is the first to be added, then the
%%% inserted record will be `{1, {hello, 123}}'.
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
-export([start_link/1, add_tuple/2, get_tuple/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([handle_continue/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, espace_tspace).
-define(TABLE_OPTS, [ordered_set, protected, named_table]).

-record(state, {inst_name, tspace_tabid, etsmgr_pid, next_key=1}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Add a new tuple to the tuple space ETS table.
%%
%% The tuple is inserted with a unique `integer()' as the key, the key
%% is incremented with every insert.
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
%% @doc Lookup a tuple pattern in the tuple space ETS table.
%%
%% If no match is found, `{nomatch}' is returned.
%%
%% If a match is found, `{match, Key, List, Tuple}' is returned, where
%% `Key' uniquely identifies the ETS record, `List' is the list of the
%% `$N' elements referenced in the pattern, if any, and `Tuple' is the
%% second part of the ETS record.
%%
%% This function runs within the client process. Since for `rd' and
%% `rdp' we do not require write access to the table, we can skip
%% calling the gen_server and read from the ETS table directly.
%%
%% For `rd' and `rdp', although we are bypassing the gen_server, we
%% still need to provide some of the `State' data to the handler. For
%% now, this is created manually, but it will be fixed more elegantly
%% in future.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_tuple(atom(), in|rd|inp|rdp, tuple()) -> {nomatch} | {nomatch, reference()} | {match, {list(), tuple()}}.
get_tuple(Inst_name, rdp, Pattern) ->
    Tab_name = espace_util:inst_to_name(espace_tspace, Inst_name),
    State = #state{inst_name=Inst_name, tspace_tabid=Tab_name},
    handle_get_tuple(State, rdp, Pattern, self());

get_tuple(Inst_name, rd, Pattern) ->
    Tab_name = espace_util:inst_to_name(espace_tspace, Inst_name),
    State = #state{inst_name=Inst_name, tspace_tabid=Tab_name},
    handle_get_tuple(State, rd, Pattern, self());

get_tuple(Inst_name, Espace_op, Pattern) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {get_tuple, Espace_op, Pattern}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% We expect an instance name to be supplied, which will be used to
%% uniquely identify the ETS table for the instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} |
                            ignore |
                            {error, {already_started, pid()} | term()}.
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
-spec init(atom()) -> ignore |
                      {ok, term(), hibernate} |
                      {ok, term(), timeout()} |
                      {ok, term(), {continue, term()}} |
                      {ok, term()} |
                      {stop, term()}.
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
-spec handle_call({get_tuple, tuple()} | {add_tuple, tuple()},
                  {pid(), term()}, term()) ->
                         {noreply, term(), hibernate | timeout() | {continue, term()}} |
                         {noreply, term()} |
                         {reply, term(), term(), hibernate | timeout() | {continue, term()}} |
                         {reply, term(), term()} |
                         {stop, term(), term(), term()} |
                         {stop, term(), term()}.
handle_call({get_tuple, Espace_op, Pattern}, _From={Cli_pid,_}, State) ->
    Reply = handle_get_tuple(State, Espace_op, Pattern, Cli_pid),
    {reply, Reply, State};

handle_call({add_tuple, Tuple}, _From, State) ->
    {Reply, State2} = handle_add_tuple(Tuple, State),
    {reply, Reply, State2};

handle_call(Request, From, State) ->
    logger:warning("~p:handle_call: Unexpected request=~p, from pid=~p, ignored.",
                   [?SERVER, Request, From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(Request, State) ->
    logger:warning("~p:handle_cast: Unexpected request=~p, ignored.",
                   [?SERVER, Request]),
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
-spec handle_continue(term(), term()) -> {noreply, term(), hibernate} |
                                         {noreply, term(), timeout()} |
                                         {noreply, term(), {continue, term()}} |
                                         {noreply, term()} |
                                         {stop, term(), term()}.
handle_continue(init, State) ->
    case handle_wait4etsmgr(init, State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Error} ->
            {stop, Error}
    end;

handle_continue(Request, State) ->
    logger:warning("~p:handle_continue: Unexpected request=~p, ignored.",
                   [?SERVER, Request]),
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
-spec handle_info(timeout() | term(), term()) -> {noreply, term(), hibernate} |
                                                 {noreply, term(), timeout()} |
                                                 {noreply, term()} |
                                                 {stop, normal | term(), term()}.
handle_info({'EXIT', Pid, Reason}, State) ->
    Mgr_pid = State#state.etsmgr_pid,
    case Pid of
        Mgr_pid ->
            logger:warning("~p: etsmgr (~p) has died, reason=~p, waiting for restart.",
                           [?SERVER, Pid, Reason]),
            case handle_wait4etsmgr(recover, State) of
                {ok, State2} ->
                    logger:info("~p: etsmgr has recovered.", [?SERVER]),
                    {noreply, State2};
                {error, Error} ->
                    {stop, Error}
            end;
        _Other_pid ->
            logger:warning("~p: unexpected EXIT from pid=~p, reason=~p, ignored.",
                           [?SERVER, Pid, Reason]),
            {noreply, State}
    end;

handle_info({'ETS-TRANSFER', Table_id, From_pid, Gift_data}, State) ->
    logger:info("~p:ETS-TRANSFER tabid=~p, from=~p, gift_data=~p.", [?SERVER, Table_id, From_pid, Gift_data]),
    {noreply, State};

handle_info(Info, State) ->
    logger:warning("~p:handle_info: Unexpected message=~p, ignored.", [?SERVER, Info]),
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
terminate(Reason, State) ->
    logger:info("~p: terminating, reason=~p, state=~p.", [?SERVER, Reason, State]),
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
-spec handle_get_tuple(term(), in|rd|inp|rdp, tuple(), pid()) ->
                              {nomatch} | {nomatch, reference()} | {match, {list(), tuple()}}.
handle_get_tuple(State, Espace_op, Pattern, Cli_pid) ->
    TabId = State#state.tspace_tabid,
    Match = ets:match(TabId, {'$0', Pattern}, 1),
    case Match of
        '$end_of_table' ->  %% no match
            case Espace_op of
                inp ->
                    {nomatch};
                rdp ->
                    {nomatch};
                _ -> %% only "in" and "rd" should block on no match
                    Cli_ref = make_ref(), %% the client should wait for this ref
                    Inst_name = State#state.inst_name,
                    espace_tspatt_srv:add_pattern(Inst_name, Cli_ref, Pattern, Cli_pid),
                    {nomatch, Cli_ref}
            end;

        {[[TabKey|Fields]],_Continuation} -> %% We only want one match, and we ignore the ets:match continuation
            [{TabKey, Tuple}] = ets:lookup(TabId, TabKey), %% we always also return the whole tuple
            case Espace_op of   %% "in" and "inp" should remove the tuple
                in ->
                    ets:delete(TabId, TabKey);
                inp ->
                    ets:delete(TabId, TabKey);
                _ ->
                    ok
            end,
            {match, {Fields, Tuple}} %% Fields may contain data, if Pattern had '$N'

    end.


%%--------------------------------------------------------------------
%% @private
%% @doc add_tuple handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_tuple(tuple(), term()) -> term().
handle_add_tuple(Tuple, State) ->
    Inst_name = State#state.inst_name,
    Tab_id = State#state.tspace_tabid,
    Tab_key = State#state.next_key,
    ets:insert(Tab_id, {Tab_key, Tuple}),
    State2 = State#state{next_key=Tab_key+1},
    espace_tspatt_srv:check_waitlist(Inst_name, Tuple),
    {done, State2}.


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
            Next_key = get_next_key(Table_id),
            {ok, State#state{etsmgr_pid=Mgr_pid, tspace_tabid=Table_id, next_key=Next_key}};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Returns the last key used in a table.
%%
%% This is only needed during the application startup, and when the
%% server has restarted by the supervisor following a crash.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_next_key(ets:tid()) -> integer().
get_next_key(Tab_id) ->
    case ets:select(Tab_id, [{ {'$1', {'_'}}, [], ['$$'] }]) of
        [] ->
            1;
        Keys ->
            1+lists:max(lists:flatten(Keys))
    end.
