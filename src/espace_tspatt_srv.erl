%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% Custodian for the `espace_tspatt', waiting patterns, ETS table.
%%%
%%% The table is created as an `ordered_set' and in `protected'
%%% mode. All access to the table is expected to come through this
%%% server. However, other proceses can inspect its contents for
%%% debugging purposes.
%%%
%%% The table keeps track of client processes that are blocked on `in'
%%% or `rd' waiting for a tuple matching their pattern to be added to
%%% the tuple space. In effect the ETS table is a pattern waiting
%%% list.
%%%
%%% Our sole client is `espace_tspace_srv'. Whenever an `in' or `rd'
%%% operation does not find a match the client is given a unique key
%%% to wait on, and that key along with the pattern and client's pid
%%% is passed to us, via `add_pattern/4', to add to the waiting list.
%%%
%%% Whenever a new tuple is added to the tuple space, we will receive
%%% a copy of the tuple, via `check_waitlist/2', to check against
%%% waiting patterns. If we find a match, the correponding client(s)
%%% will be notified of the new arrival.
%%%
%%% Communication from `espace_tspace_srv' is unidirectional. Once it
%%% sends us a request, it will continue with its own work. We never
%%% reply to `espace_tspace_srv'.
%%%
%%% The ETS table name used will reflect the `espace' instance
%%% name. This will be `espace_tspatt' for the default/unnamed instance, and
%%% `espace_tspatt_abc' for an instance named `abc'.
%%%
%%% The `etsmgr' application is used to add resilience to the server
%%% data, should the server restart while it is holding tuple
%%% patterns.
%%%
%%% @end
%%% Created : 12 Jan 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_tspatt_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, check_waitlist/2, add_pattern/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([handle_continue/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, espace_tspatt).
-define(TABLE_OPTS, [ordered_set, protected]).
-define(TABLE_IDKEY, tspatt_tabid).

-record(state, {inst_name, tspatt_tabid, etsmgr_pid}).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-define(Log_base, #{fun_name=>?FUNCTION_NAME}).
-define(Log_info(Log_map), ?LOG_INFO(maps:merge(?Log_base, Log_map))).
-define(Log_warning(Log_map), ?LOG_WARNING(maps:merge(?Log_base, Log_map))).

%%--------------------------------------------------------------------

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Check a tuple against waiting patterns.
%%
%% We check the newly added tuple against the existing client patterns
%% using `ets:test_ms/2'. If a pattern is found, the waiting client(s)
%% will be informed, via their `Pid' and `Cli_Ref', to retry the
%% `in' or `rd' operation.
%%
%% Once a client is notified, the pattern will be removed from the
%% waiting list.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_waitlist(atom(), tuple()) -> ok.
check_waitlist(Inst_name, Tuple) ->
    gen_server:cast(espace_util:inst_to_name(?SERVER, Inst_name), {check_tuple, Tuple}).

%%--------------------------------------------------------------------
%% @doc Add a new pattern to the waiting list.
%%
%% We insert the `Pattern' along with the client's pid and unique ref
%% in the ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_pattern(atom(), reference(), tuple(), pid()) -> ok.
add_pattern(Inst_name, Cli_ref, Pattern, Cli_pid) ->
    gen_server:cast(espace_util:inst_to_name(?SERVER, Inst_name), {add_pattern, Cli_ref, Pattern, Cli_pid}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% We expect an instance name to be supplied, which will be used to
%% uniquely identify the ETS table for the instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) ->
          ignore |
          {error, {already_started, pid()} | term()} |
          {ok, pid()}.
start_link(Inst_name) ->
    Server_name = espace_util:inst_to_name(?SERVER, Inst_name),
    gen_server:start_link({local, Server_name}, ?MODULE, Inst_name, []).

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
-spec init(atom()) -> {ok, term(), {continue, init}}.
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
-spec handle_call(term(), {pid(), term()}, term()) -> {reply, ok, term()}.
handle_call(Request, From, State) ->
    ?Log_warning(#{text=>"Unexpected request - ignored",
                   server=>?SERVER, request=>Request, from=>From}),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast({atom(), tuple()}, term()) -> {noreply, term()}.
handle_cast({check_tuple, Tuple}, State) ->
    handle_check_tuple(State#state.tspatt_tabid, Tuple),
    {noreply, State};

handle_cast({add_pattern, Cli_ref, Pattern, Cli_pid}, State) ->
    handle_add_pattern(State#state.tspatt_tabid, {Cli_ref, Pattern, Cli_pid}),
    {noreply, State};

handle_cast(Request, State) ->
    ?Log_warning(#{text=>"Unexpected request - ignored.",
                   server=>?SERVER, request=>Request}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling continue requests.
%%
%% We use `{continue, init}' in `espace_tspatt_srv:init/1' to ensure that
%% `etsmgr' is started and is managing our ETS table before handling
%% the first request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(term(), term()) ->
          {noreply, term(), {continue, term()} } |
          {noreply, term()}.
handle_continue(init, State) ->
    case handle_wait4etsmgr(init, State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Error} ->
            {stop, Error}
    end;

handle_continue(Request, State) ->
    ?Log_warning(#{text=>"Unexpected request - ignored.",
                   server=>?SERVER, request=>Request}),
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
-spec handle_info(timeout() | term(), term()) ->
          {noreply, term()} |
          {stop, normal | term(), term()}.
handle_info({'EXIT', Pid, Reason}, State) ->
    Mgr_pid = State#state.etsmgr_pid,
    case Pid of
        Mgr_pid ->
            ?Log_warning(#{text=>"etsmgr has died - waiting for restart.",
                           server=>?SERVER, pid=>Pid, reason=>Reason}),
            case handle_wait4etsmgr(recover, State) of
                {ok, State2} ->
                    ?Log_info(#{text=>"etsmgr has recovered.", server=>?SERVER}),
                    {noreply, State2};
                {error, Error} ->
                    {stop, Error}
            end;
        _Other_pid ->
            ?Log_warning(#{text=>"unexpected EXIT - ignored",
                           server=>?SERVER, pid=>Pid, reason=>Reason}),
            {noreply, State}
    end;

handle_info({'ETS-TRANSFER', Table_id, From_pid, Gift_data}, State) ->
    ?Log_info(#{text=>"ETS-TRANSFER", server=>?SERVER, tabid=>Table_id,
                from=>From_pid, gift=>Gift_data}),
    {noreply, State};

handle_info(Info, State) ->
    ?Log_warning(#{text=>"Unexpected message - ignored.",
                   server=>?SERVER, info=>Info}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% Before terminating, a `quit' message is sent to all clients blocked
%% in a `in' or `rd'.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(atom(), term()) -> ok.
terminate(Reason, State) ->
    ?Log_info(#{text=>"terminating", server=>?SERVER,
                reason=>Reason, state=>State}),
    Inst_name = State#state.inst_name,

    Send_quit = fun ({Cli_ref, _Pattern, Cli_pid}, Acc) ->
                        Cli_pid ! {Cli_ref, quit},
                        Acc
                end,
    ets:foldl(Send_quit, 0, State#state.tspatt_tabid),

    Table_name = espace_util:inst_to_name(?TABLE_NAME, Inst_name),
    etsmgr:del_table(Inst_name, Table_name),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% check tuple against current pattern being waited for by `in' or
%% `rd' clients.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_tuple(tuple(), ets:tid(), atom()) -> none.
check_tuple(_Tuple, _TabId, '$end_of_table') ->
    none;
check_tuple(Tuple, TabId, Key) ->
    [{Cli_ref, Pattern, Cli_pid}] = ets:lookup(TabId, Key),
    case ets:test_ms(Tuple, [{Pattern,[],['$$']}]) of
        {ok, false} ->
            nomatch;
        _ ->
            Cli_pid ! {Cli_ref, retry},
            ets:delete(TabId, Key)
    end,
    check_tuple(Tuple, TabId, ets:next(TabId, Key)).

%%--------------------------------------------------------------------
%% @private
%% @doc handle the check_tuple cast.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_check_tuple(ets:tid(), tuple()) -> true.
handle_check_tuple(TabId, Tuple) ->
    ets:safe_fixtable(TabId, true), % we may be deleting records while scanning
    check_tuple(Tuple, TabId, ets:first(TabId)),
    ets:safe_fixtable(TabId, false).

%%--------------------------------------------------------------------
%% @private
%% @doc handle the add_pattern cast.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_add_pattern(ets:tid(), tuple()) -> true.
handle_add_pattern(TabId, Tuple) ->
    ets:insert(TabId, Tuple).

%%--------------------------------------------------------------------
%% @doc wait for etsmgr to (re)start, ensure it manages our ETS table,
%% and update the `State' data.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_wait4etsmgr(atom(), term()) ->
          {ok, term()} |
          {error, term()}.
handle_wait4etsmgr(Mode, State) ->
    Inst_name = State#state.inst_name,
    Table_name = espace_util:inst_to_name(?TABLE_NAME, Inst_name),

    Result = case Mode of
                 init ->
                     espace_util:wait4etsmgr(Inst_name, init, Table_name, ?TABLE_OPTS);
                 recover ->
                     espace_util:wait4etsmgr(Inst_name, recover, Table_name, State#state.tspatt_tabid)
             end,

    case Result of
        {ok, Mgr_pid, Table_id} ->
            espace_pterm:put(Inst_name, ?TABLE_IDKEY, Table_id),
            {ok, State#state{etsmgr_pid=Mgr_pid, tspatt_tabid=Table_id}};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
