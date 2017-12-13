%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor
%% Taken, unashemedly, from LYSE.
%% @spec
%% @end
%%--------------------------------------------------------------------
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
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
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
