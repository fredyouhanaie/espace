% -*- indent-tabs-mode:nil; -*-
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

-include_lib("kernel/include/logger.hrl").

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
%% When named instances are started, a number of instance specific
%% `persistent_term' entries are created. We ensure these are removed
%% on exit.
%%
%% The `persistent_term' entries are expected to have the following
%% format: `{espace, Inst_name, Prefix}', where, `Prefix' identifies
%% the particular `espace' item prefix, such as server name to ETS
%% table name, and `Inst_name' is the espace instance name, which is
%% the same as the application name.
%%
%% No entries are created for the unnamed instance, whose application
%% name is `espace'.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    case application:get_application() of
        {ok, espace} ->
            ok;
        {ok, App_name} -> %% this is in fact Inst_name
            My_term = fun ({{espace, Inst_name, _}, _V})
                            when Inst_name == App_name ->
                              true;
                          (_) ->
                              false
                      end,
            App_terms = lists:filter(My_term, persistent_term:get()),
            lists:foreach(fun ({K, _V}) -> persistent_term:erase(K) end, App_terms);
        undefined ->
            ?LOG_WARNING("~p: stop called in non-application context.", [?MODULE])
        end,
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
