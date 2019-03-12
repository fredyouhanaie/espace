%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
%%% Main espace application supervisor.
%%% It handles the main application components.
%%%
%%% There are four child components, three are gen_servers, and the
%%% fourth is a supervisor. See the main overview for details.
%%%
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the main supervisor.
%%
%% We expect an espace instance name, which will uniquely identify all
%% the components of the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) ->
			{ok, pid()} |
			ignore |
			{error, {already_started, pid()} | {shutdown, term()} | term()}.
start_link(Inst_name) ->
    Server_name = espace_util:inst_to_name(?SERVER, Inst_name),
    supervisor:start_link({local, Server_name}, ?MODULE, Inst_name).

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
-spec init(atom()) -> {ok, {map(), [map()]}}.
init(Inst_name) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Children = [
		#{id => 'tspool_srv',
		  start => {'tspool_srv', start_link, [Inst_name]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => ['tspool_srv']},

		#{id => 'tspace_srv',
		  start => {'tspace_srv', start_link, [Inst_name]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => ['tspace_srv']},

		#{id => 'tspatt_srv',
		  start => {'tspatt_srv', start_link, [Inst_name]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => ['tspatt_srv']},

		#{id => 'worker_sup',
		  start => {'worker_sup', start_link, [Inst_name]},
		  restart => permanent,
		  shutdown => 5000,
		  type => supervisor,
		  modules => ['wkpool_srv']}

	       ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
