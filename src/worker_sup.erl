%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata,co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% 
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata,co.uk>
%%%-------------------------------------------------------------------
-module(worker_sup).

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
%% stop the supervisor.
%% Unashemdly taken from "Learn You Some Erlang"
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------

stop() ->
    case whereis(?SERVER) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ -> ok
end.

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

    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,  %% TODO needs tuning
		 period => 5},    %% TODO needs tuning

    AChild = #{id => tsworker,
	       start => {erlang, apply, []}, %% TODO need own wrapper here
	       shutdown => brutal_kill,
	       type => worker},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
