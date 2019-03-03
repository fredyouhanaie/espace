%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% Main application module.
%%%
%%% Multiple instances of the application can be active concurrently
%%% within the same node, and without interfering with each
%%% other. Each instance will be identified by a unique instance name,
%%% an atom.
%%%
%%% The instance name `espace' has a special meaning as it identifies
%%% the unnamed instance. The `espace' instance can be started with
%%% `application:start(espace)'. The application description will be
%%% taken from the `espace.app' file.
%%%
%%% Starting a named instance is slightly more involved, since an
%%% appropriate app description needs to be created and passed to
%%% `application:load/1', followed by `application:start/1'.
%%%
%%% To start an instance of espace `espace:start/0,1' should be
%%% used. The started application will have the same name as the
%%% instance, or `espace', in the case of the unnamed instance.
%%%
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(normal, atom()) ->
		   {ok, pid()} |
		   ignore |
		   {error, {already_started, pid()} | {shutdown, term()} | term()}.
start(normal, Inst_name) ->
    espace_sup:start_link(Inst_name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
