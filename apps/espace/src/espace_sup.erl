%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
%%% Main espace application supervisor.
%%% It handles the main application components, TSPOOL and WKPOOL.
%%%
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> ({ok, pid()} | ignore | {error, any()}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok',{#{'intensity':=1, 'period':=5, 'strategy':='one_for_one'},[map(),...]}}.
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Children = [

		#{id => tspool_sup,
		  start => {tspool_sup, start_link, []},
		  restart => permanent,
		  shutdown => 5000,
		  type => supervisor,
		  modules => [tspool_sup]},

		#{id => wkpool_sup,
		  start => {wkpool_sup, start_link, []},
		  restart => permanent,
		  shutdown => 5000,
		  type => supervisor,
		  modules => [wkpool_sup]}
		],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
