%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% This is the main admin manager for `espace' instances. It is the
%%% custodian of the two ETS tables and replaces the table admin side
%%% of the `espace_tspace_srv' and `espace_tspatt_srv' servers.
%%%
%%% There will be one such server for each instance of `espace'. The
%%% server communicates with the `etsmgr' server and handles the setup
%%% and teardown of the ETS tables during the startup and shutdown
%%% phases.
%%%
%%% This server does not deal with the data input/output side of
%%% `espace'. The tables are created with public access mode so that
%%% individual processes can perform their own input/output
%%% operations, see the `espace_cli' module for details.
%%%
%%% @end
%%% Created : 01 Jun 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_mgr).

-behaviour(gen_server).

-include_lib("include/espace.hrl").

%%--------------------------------------------------------------------

%% API
-export([start_link/1, wait_for_ets/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------

-record(state, {inst_name, tspace_tabid, tspatt_tabid, etsmgr_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% We expect an instance name to be supplied, which will be used to
%% uniquely identify the ETS tables for the instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) ->
          {ok, pid()} |
          ignore |
          {error, {already_started, pid()} | term()}.
start_link(Inst_name) ->
    gen_server:start_link({local, espace_util:inst_to_name(?SERVER, Inst_name)},
                          ?MODULE, Inst_name, []).

%%--------------------------------------------------------------------
%% @doc Allows the client(s) to wait for the tables to be ready.
%%
%% This call will not be serviced until the tables have been created
%% via the `{continue,init}' handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_ets(atom()) -> true.
wait_for_ets(Inst_name) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), wait_for_ets).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
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
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()}.
handle_call(wait_for_ets, _From, State) ->
    {reply, true, State};
handle_call(Request, From, State) ->
    Reply = ok,
    ?Log_warning(#{text=>"Unexpected request - ignored.",
                   server=>?SERVER, request=>Request, from=>From}),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()}.
handle_cast(Request, State) ->
    ?Log_warning(#{text=>"Unexpected request - ignored.",
                   server=>?SERVER, request=>Request}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling continue requests.
%%
%% We use `{continue, init}' from `espace_mgr:init/1' to ensure
%% that `etsmgr' is started and is managing our ETS tables before
%% handling the first request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(term(), term()) ->
          {noreply, term(), hibernate} |
          {noreply, term(), timeout()} |
          {noreply, term(), {continue, term()}} |
          {noreply, term()} |
          {stop, term(), term()}.
handle_continue(init, State) ->
    ?Log_info(#{text=>"continue/init", server=>?SERVER, state=>State}),
    case handle_wait4etsmgr(init, State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Error} ->
            {stop, Error, State}
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
            handle_wait4etsmgr(recover, State);
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
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), term()) -> ok.
terminate(Reason, State) ->
    ?Log_info(#{text=>"terminating", server=>?SERVER, reasone=>Reason,
                state=>State}),
    Inst_name = State#state.inst_name,

    etsmgr:del_table(Inst_name,
                     espace_util:inst_to_name(?Table_param(tspace, name), Inst_name)),

    Send_quit = fun ({_Key, Cli_ref, _Pattern, Cli_pid}, Acc) ->
                        Cli_pid ! {Cli_ref, quit},
                        Acc
                end,
    ets:foldl(Send_quit, 0, State#state.tspatt_tabid),

    etsmgr:del_table(Inst_name,
                     espace_util:inst_to_name(?Table_param(tspatt, name), Inst_name)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc wait for etsmgr to (re)start, ensure it manages our ETS
%% tables, and update the `State' data.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_wait4etsmgr(init|recover, term()) ->
          {ok, term()} | {error, term()}.
handle_wait4etsmgr(init, State) ->
    Inst_name = State#state.inst_name,
    etsmgr:wait4etsmgr(Inst_name),

    case ets_init(Inst_name, tspace) of
        {ok, Mgr_pid, TSPACE_tabid} ->
            case ets_init(Inst_name, tspatt) of
                {ok, Mgr_pid, TSPATT_tabid} ->
                    {ok, State#state{etsmgr_pid=Mgr_pid,
                                     tspace_tabid=TSPACE_tabid,
                                     tspatt_tabid=TSPATT_tabid}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

handle_wait4etsmgr(recover, State) ->
    Inst_name = State#state.inst_name,
    etsmgr:wait4etsmgr(Inst_name),

    case ets_recover(Inst_name, tspace) of
        {ok, Mgr_pid, TSPACE_tabid} ->
            case ets_recover(Inst_name, tspatt) of
                {ok, Mgr_pid, TSPATT_tabid} ->
                    {ok, State#state{etsmgr_pid=Mgr_pid,
                                     tspace_tabid=TSPACE_tabid,
                                     tspatt_tabid=TSPATT_tabid}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Create a single ETS table via etsmgr.
%%
%% @end
%%--------------------------------------------------------------------
-spec ets_init(atom(), tspace|tspatt) ->
          {ok, pid(), ets:tid()} | {error, term()}.
ets_init(Inst_name, Table) ->
    Table_name = espace_util:inst_to_name(Inst_name, ?Table_param(Table, name)),
    Table_opts = ?Table_param(Table, opts),
    case etsmgr:new_table(Inst_name, Table_name, Table_name, Table_opts) of
        {ok, Mgr_pid, Table_id} ->
            espace_pterm:put(Inst_name, ?Table_param(Table, idkey), Table_id),
            {ok, Mgr_pid, Table_id};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Recover from etsmgr crash.
%%
%% @end
%%--------------------------------------------------------------------
-spec ets_recover(atom(), tspace|tspatt) ->
          {ok, pid(), ets:tid()} | {error, term()}.
ets_recover(Inst_name, Table) ->
    Table_name = espace_util:inst_to_name(Inst_name, ?Table_param(Table, name)),
    Table_id = espace_pterm:get(Inst_name, ?Table_param(Table, idkey)),

    case etsmgr:add_table(Inst_name, Table_name, Table_id) of
        {ok, Mgr_pid, Table_id} ->
            espace_pterm:put(Inst_name, ?Table_param(Table, idkey), Table_id),
            {ok, Mgr_pid, Table_id};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
