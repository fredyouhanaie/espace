%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% Worker Pool supervisor. Supervises the gen_server and the supervisor of the workers.
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wkpool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, stop/0]).

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
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor
%% Taken, unashemedly, from LYSE.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok' | 'true'.
stop() ->
    case whereis(?SERVER) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ -> ok
    end.

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
-spec init([]) -> {'ok',{#{'intensity':=1, 'period':=5, 'strategy':='one_for_all'},[map(),...]}}.
init([]) ->

    SupFlags = #{strategy => one_for_all,
		 intensity => 1,
		 period => 5},

    Children = [
		#{id => wkpool_srv,
		  start => {wkpool_srv, start_link, []},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [wkpool_srv]},
		#{id => worker_sup,
		  start => {worker_sup, start_link, []},
		  restart => permanent,
		  shutdown => 5000,
		  type => supervisor,
		  modules => [wkpool_srv]}
	       ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
