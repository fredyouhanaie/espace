%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata,co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% Supervises the "eval" worker processes.
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata,co.uk>
%%%-------------------------------------------------------------------
-module(worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, run_child/3, run_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) ->
			{ok, pid()} |
			ignore |
			{error, {already_started, pid()} | {shutdown, term()} | term()}.
start_link(Inst_name) ->
    supervisor:start_link({local, espace:inst_to_name(?SERVER, Inst_name)}, ?MODULE, Inst_name).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(atom()) -> {ok, {map(), [map()]}}.
init(_Inst_name) ->

    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,  %% TODO needs tuning
		 period => 5},    %% TODO needs tuning

    AChild = #{id => tsworker,
	       start => {?MODULE, run_child, []},
	       restart => temporary,
	       shutdown => brutal_kill,
	       type => worker},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec run_child(atom(), atom(), list()) -> {ok, pid()}.
run_child(M, F, A) ->
    {ok, spawn_link(M, F, A)}.

-spec run_child(string() | function(), list()) -> {ok, pid()}.
run_child(Fun, Args) when is_function(Fun) ->
    {ok, spawn_link(erlang, apply, [Fun, Args])};

run_child(Fun, Args) ->
    {ok, Tokens, _} = erl_scan:string(Fun),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, F, _} = erl_eval:exprs(Parsed, []),
    {ok, spawn_link(erlang, apply, [F, Args])}.
