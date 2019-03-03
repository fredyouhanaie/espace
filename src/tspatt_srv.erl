%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% Custodian for the tspatt, waiting patterns, ETS table.
%%%
%%% The table is created as a `set' and in `protected' mode. All
%%% access to the table is expected to come through this server.
%%% However, other proceses can inspect the contents of the table for
%%% debugging purposes.
%%%
%%% The table keeps track of client processes that are blocked on `in'
%%% or `rd' waiting for a tuple matching their pattern to be added to
%%% the tuple space. In effect the ETS table is a pattern waiting
%%% list.
%%%
%%% Our sole client is `tspace_srv'. Whenever an `in' or `rd'
%%% operation does not find a match the client is given a unique key
%%% to wait on, and that key along with the pattern and client's pid
%%% is passed to us, via `add_pattern/4', to add to the waiting list.
%%%
%%% Whenever a new tuple is added to the tuple space, we will receive
%%% a copy of the tuple, via `check_waitlist/2', to check against
%%% waiting patterns. If we find a match, the correponding client(s)
%%% will be notified of the new arrival.
%%%
%%% Communication from `tspace_srv' is unidirectional. Once it sends
%%% us a request, it will continue with its own work. We never reply
%%% to `tspace_srv'.
%%%
%%% The ETS table name used will reflect the `espace' instance
%%% name. This will be `tspatt' for the default/unnamed instance, and
%%% `tspatt_abc' for an instance named `abc'.
%%%
%%% @end
%%% Created : 12 Jan 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tspatt_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, check_waitlist/2, add_pattern/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {inst_name, tspatt}).

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
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {check_tuple, Tuple}).

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
    gen_server:cast(espace:inst_to_name(?SERVER, Inst_name), {add_pattern, Cli_ref, Pattern, Cli_pid}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% We expect an instance name to be supplied, which will be used to
%% uniquely identify the ETS table for the instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> ignore |
			    {error, {already_started, pid()} | term()} |
			    {ok, pid()}.
start_link(Inst_name) ->
    Server_name = espace:inst_to_name(?SERVER, Inst_name),
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
-spec init(atom()) -> {ok, term()}.
init(Inst_name) ->
    process_flag(trap_exit, true),
    Patt_name = espace:inst_to_name(tspatt, Inst_name),
    Patt = ets:new(Patt_name, [set, protected]),
    {ok, #state{inst_name=Inst_name, tspatt=Patt}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), pid(), term()) -> {reply, ok, term()}.
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
-spec handle_cast({atom(), tuple()}, term()) -> {noreply, term()}.
handle_cast({check_tuple, Tuple}, State) ->
    TSpatt = State#state.tspatt,
    ets:safe_fixtable(TSpatt, true), % we may be deleting records while scanning
    check_tuple(Tuple, TSpatt, ets:first(TSpatt)),
    ets:safe_fixtable(TSpatt, false),
    {noreply, State};

handle_cast({add_pattern, Cli_ref, Pattern, Cli_pid}, State) ->
    ets:insert(State#state.tspatt, {Cli_ref, Pattern, Cli_pid}),
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
-spec terminate(atom(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% check tuple against current pattern being waited for, by in or rd
%% clients.
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
	    Cli_pid ! Cli_ref,
	    ets:delete(TabId, Key)
    end,
    check_tuple(Tuple, TabId, ets:next(TabId, Key)).
