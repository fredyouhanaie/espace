% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% This is the server that manages the Tuple Space.
%%%
%%% It is a `gen_server' that sits between the `espace' clients and
%%% the rest of the espace application.
%%%
%%% Upon receiving the `worker' request, the worker supervisor will be
%%% triggered to start a new child process to run the supplied
%%% function.
%%%
%%% Upon receiving the `eval' request, the worker supervisor will be
%%% triggered to start a new child process to evaluate the supplied
%%% tuple.
%%%
%%% For the tuple space data operations, i.e. `in', `rd', `inp', `rdp'
%%% and `out', the request is passed to the `espace_tspace_srv'
%%% server, which in turn will process the request.
%%%
%%% The patterns supplied with the input operators should be
%%% compatible with `ets:match/2,3' match patterns. When match is
%%% found the return results are in the form of `{List, Tuple}', where
%%% `List' is the, possibaly empty, list of the matched `$N' pattern
%%% variables, and `Tuple' is the entire data tuple.
%%%
%%% @end
%%% Created :  9 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_tspool_srv).

-behaviour(gen_server).

%% API
-export([start_link/1, espace_eval/2, espace_out/2, espace_in/2, espace_rd/2, espace_inp/2, espace_rdp/2, espace_worker/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {inst_name, workersup}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> ignore |
                            {error, {already_started, pid()} | term()}|
                            {ok, pid()}.
start_link(Inst_name) ->
    gen_server:start_link({local, espace_util:inst_to_name(?SERVER, Inst_name)}, ?MODULE, Inst_name, []).

%%--------------------------------------------------------------------
%% @doc
%% perform an `eval' operation.
%%
%% A request is sent to the worker supervisor to start a new child
%% process, which will evaluate the elements of the supplied `Tuple'.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_eval(atom(), tuple()) -> pid().
espace_eval(Inst_name, Tuple) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {espace_eval, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% start a worker process.
%%
%% A request is sent to the worker supervisor to start a new child
%% process based on the `MFA' parameter. `MFA' can be one of `{Mod,
%% Func, Args}' triple, or an anonymous function/args pair `{Func,
%% Args}', where `Func' can be a `fun' expression or string
%% represtation of a `fun' expression. The string version can be used
%% when reading espace terms from a file.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_worker(atom(), tuple()) -> pid().
espace_worker(Inst_name, MFA) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {espace_worker, MFA}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `out' operation.
%%
%% Sends supplied `Tuple' to `espace_tspace_srv' to be stored in the
%% ETS table.
%%
%% `espace_tspace_srv' will in turn trigger `espace_tspatt_srv' to
%% inform any `in'/`rd' clients that may be blocking on such
%% tuple. The blocking mechanisim is implemented internally in this
%% module, see `espace_op/3'.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_out(atom(), tuple()) -> done.
espace_out(Inst_name, Tuple) ->
    gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {espace_out, Tuple}).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `in' operation.
%%
%% The request is passed to `espace_tspace_srv' to search for
%% `Pattern', if found, the data is returned, otherwise we block until
%% a matching tuple is added to the tuple space via `out'. The matched
%% tuple will be removed from the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_in(atom(), tuple()) -> {list(), tuple()}.
espace_in(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_in, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rd' operation.
%%
%% Similar to `in', except that the matched tuple remains in the tuple
%% space.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rd(atom(), tuple()) -> {list(), tuple()}.
espace_rd(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_rd, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `inp' operation.
%%
%% Similar to `in' but does not block if no match found. In case of no
%% match, the `nomatch' atom is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_inp(atom(), tuple()) -> nomatch | {list(), tuple()}.
espace_inp(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_inp, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rdp' operation.
%%
%% Similar to `inp', but does not remove the matched tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_rdp(atom(), tuple()) -> nomatch | {list(), tuple()}.
espace_rdp(Inst_name, Pattern) ->
    espace_op(Inst_name, espace_rdp, Pattern).

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
-spec init(atom()) -> {ok, #state{inst_name::atom(), workersup::atom()}}.
init(Inst_name) ->
    process_flag(trap_exit, true),
    {ok, #state{inst_name=Inst_name, workersup=espace_util:inst_to_name(espace_worker_sup, Inst_name)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call({atom(), tuple()}, pid(), term()) ->
                         {reply,
                          {nomatch} | {nomatch, reference()} | {match, {list(), tuple()}},
                          term()
                         }.
handle_call({espace_in, Pattern}, From, State) ->
    handle_espace_op(espace_in, Pattern, From, State);

handle_call({espace_inp, Pattern}, From, State) ->
    handle_espace_op(espace_inp, Pattern, From, State);

handle_call({espace_rd, Pattern}, From, State) ->
    handle_espace_op(espace_rd, Pattern, From, State);

handle_call({espace_rdp, Pattern}, From, State) ->
    handle_espace_op(espace_rdp, Pattern, From, State);

handle_call({espace_out, Tuple}, _From, State) ->
    Reply = espace_tspace_srv:add_tuple(State#state.inst_name, Tuple),
    {reply, Reply, State};

handle_call({espace_eval, Tuple}, _From, State) ->
    Reply = handle_espace_eval(State#state.inst_name, State#state.workersup, Tuple),
    {reply, Reply, State};

handle_call({espace_worker, {M, F, A}}, _From, State) ->
    Reply = handle_espace_worker(State#state.workersup, {M, F, A}),
    {reply, Reply, State};

handle_call({espace_worker, {Fun, Args}}, _From, State) ->
    Reply = handle_espace_worker(State#state.workersup, {Fun, Args}),
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
-spec terminate(atom(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% unified function for "in" and "rd" client API functions
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_op(atom(), espace_in | espace_inp | espace_rd | espace_rdp, tuple()) ->
                       {list(), tuple()} | nomatch.
espace_op(Inst_name, Espace_Op, Pattern) ->
    Reply = gen_server:call(espace_util:inst_to_name(?SERVER, Inst_name), {Espace_Op, Pattern}),
    case Reply of
        {match, Match} ->
            Match;
        {nomatch} -> %% only from the inp and rdp operations
            nomatch;
        {nomatch, Cli_ref} -> %% only from the in and rd operations
            receive
                Cli_ref ->
                    espace_op(Inst_name, Espace_Op, Pattern) % our tuple has arrived, try again!
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% unified call handler for "in", "inp", "rd" and "rdp"
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_espace_op(espace_in | espace_inp | espace_rd | espace_rdp, tuple(), pid(), term()) ->
                              {reply,
                               {nomatch} | {nomatch, reference()} | {match, {list(), tuple()}},
                               term()
                              }.
handle_espace_op(Espace_Op, Pattern, From, State) ->
    case espace_tspace_srv:get_tuple(State#state.inst_name, Pattern) of
        {nomatch} ->
            case Espace_Op of
                espace_inp ->
                    {reply, {nomatch}, State};
                espace_rdp ->
                    {reply, {nomatch}, State};
                _ -> %% only "in" and "rd" should block on no match
                    {Cli_pid, _} = From,  %% we use the pid to notify the client
                    Cli_ref = make_ref(), %% the client should wait for this ref
                    espace_tspatt_srv:add_pattern(State#state.inst_name, Cli_ref, Pattern, Cli_pid),
                    {reply, {nomatch, Cli_ref}, State}
            end;
        {match, {TabKey, Fields, Tuple}} ->
            Reply = {match, {Fields, Tuple}},
            case Espace_Op of   %% "in" and "inp" should remove the tuple
                espace_in ->
                    espace_tspace_srv:del_tuple(State#state.inst_name, TabKey);
                espace_inp ->
                    espace_tspace_srv:del_tuple(State#state.inst_name, TabKey);
                _ ->
                    ok
            end,
            {reply, Reply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc handle the espace_worker call.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_espace_worker(atom(), tuple()) -> pid().
handle_espace_worker(Worker_sup, Arg) ->
    {ok, Pid} = supervisor:start_child(Worker_sup, erlang:tuple_to_list(Arg)),
    Pid.

%%--------------------------------------------------------------------
%% @private
%% @doc handle the espace_eval call.
%%
%% We start a worker child, and let `espace_util:eval_out' to evaluate
%% the tuple and `out' the result.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_espace_eval(atom(), atom(), tuple()) -> pid().
handle_espace_eval(Inst_name, Worker_sup, Tuple) ->
    {ok, Pid} = supervisor:start_child(Worker_sup, [espace_util, eval_out, [Inst_name, Tuple]]),
    Pid.
