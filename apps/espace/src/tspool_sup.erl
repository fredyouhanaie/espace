%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% Supervisor for the TSPOOL gen_servers.
%%% @end
%%% Created :  9 Jan 2018 by fy <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tspool_sup).

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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Children = [ #{id => 'tspool_srv',
		   start => {'tspool_srv', start_link, []},
		   restart => permanent,
		   shutdown => 5000,
		   type => worker,
		   modules => ['tspool_srv']},
		 #{id => 'tspace_srv',
		   start => {'tspace_srv', start_link, []},
		   restart => permanent,
		   shutdown => 5000,
		   type => worker,
		   modules => ['tspace_srv']},
		 #{id => 'tspatt_srv',
		   start => {'tspatt_srv', start_link, []},
		   restart => permanent,
		   shutdown => 5000,
		   type => worker,
		   modules => ['tspatt_srv']}
	       ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
