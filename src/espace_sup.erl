%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
%%% Main espace application supervisor.
%%% It handles the main application components.
%%%
%%% There are three child components, all are `gen_server's. Two are
%%% the main application servers, and the third is the `etsmgr'
%%% application in embedded mode, i.e. there is no separate `etsmgr'
%%% application. We also make sure that `etsmgr_srv' is the first
%%% child to be started as the other two servers depend on this
%%% server.
%%%
%%% For the child specification, we rely on the default settings of:
%%% <pre>
%%%   restart  => permanent,
%%%   shutdown => 5000,
%%%   type     => worker
%%% </pre>
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
-spec start_link(atom()) -> supervisor:startlink_ret().
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
-spec init(atom()) -> {ok, {supervisor:sup_flags(),
                            [supervisor:child_spec()]}} |
                      ignore.
init(Inst_name) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Children = [
                #{id => 'etsmgr_srv',
                  start => {'etsmgr_srv', start_link, [Inst_name]},
                  modules => ['etsmgr_srv']},

                #{id => 'espace_tspace_srv',
                  start => {'espace_tspace_srv', start_link, [Inst_name]},
                  modules => ['espace_tspace_srv']},

                #{id => 'espace_tspatt_srv',
                  start => {'espace_tspatt_srv', start_link, [Inst_name]},
                  modules => ['espace_tspatt_srv']}

               ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
