%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% Worker Pool manager. Handles espace "eval" requests.
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wkpool_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, espace_eval/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {workersup}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok' | {'ok',_}.
stop() ->
    gen_server:stop(?SERVER).

%%--------------------------------------------------------------------
%% @doc
%% perform an "eval" operation.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_eval(_) -> 'ok'.
espace_eval(MFA) ->
    gen_server:cast(?SERVER, {espace_eval, MFA}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
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
-spec init([]) -> {'ok',#state{workersup::'worker_sup'}}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{workersup=worker_sup}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(_,_,_) -> {'reply','ok',_}.
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
-spec handle_cast(_,_) -> {'noreply',_}.
handle_cast(_Msg={espace_eval, {M, F, A}}, State) ->
    supervisor:start_child(State#state.workersup, [M, F, A]),
    {noreply, State};

handle_cast(_Msg={espace_eval, {Fun, Args}}, State) ->
    supervisor:start_child(State#state.workersup, [Fun, Args]),
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
-spec handle_info(_,_) -> {'noreply',_}.
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
-spec terminate(_,_) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(_,_,_) -> {'ok',_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
