%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% This module provides the client access to the user applications.
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace).

%% API
-export([eval/1, in/1, inp/1, out/1, rd/1, rdp/1, infile/1, start/0, stop/0]).
-export([eval/2, in/2, inp/2, out/2, rd/2, rdp/2, infile/2, start/1, stop/1]).
-export([inst_to_name/2]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%%
%% Start a new instance of espace.
%%
%% start/0 will start the default instance (espace)
%% start/1 will start a named instance application.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    application:start(espace).

-spec start(atom()) -> ok | {error, term()}.
start(espace) ->
    application:start(espace);
start(Inst_name) ->
    Server_names = lists:map(fun (S) -> inst_to_name(S, Inst_name) end,
			     [ espace_sup, tspool_srv, tspace_srv, tspatt_srv, worker_sup ]
			    ),
    App = {
	   application, Inst_name,
	   [
	    {description, "An instance of espace"},
	    {vsn, "0.3.0"},
	    {registered, Server_names},
	    {mod, { espace_app, Inst_name }}
	   ]},
    application:load(App),
    application:start(Inst_name).

%%--------------------------------------------------------------------
%% @doc
%%
%% Stop an instance of espace.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(espace).

-spec stop(atom()) -> ok | {error, term()}.
stop(Inst_name) ->
    application:stop(Inst_name).


%%--------------------------------------------------------------------
%% @doc
%% Perform an "eval" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval(tuple()) -> 'ok'.
eval(MFA) when is_tuple(MFA) ->
    eval(espace, MFA),
    ok.

-spec eval(atom(), tuple()) -> 'ok'.
eval(Inst_name, MFA) when is_tuple(MFA) ->
    tspool_srv:espace_eval(Inst_name, MFA),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec in(tuple()) -> any().
in(Pattern) ->
    in(espace, Pattern).

-spec in(atom(), tuple()) -> any().
in(Inst_name, Pattern) ->
    tspool_srv:espace_in(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "inp" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec inp(tuple()) -> any().
inp(Pattern) when is_tuple(Pattern) ->
    inp(espace, Pattern).

-spec inp(atom(), tuple()) -> any().
inp(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_inp(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec out(tuple()) -> 'ok'.
out(Tuple) when is_tuple(Tuple) ->
    out(espace, Tuple).

-spec out(atom(), tuple()) -> 'ok'.
out(Inst_name, Tuple) when is_tuple(Tuple) ->
    tspool_srv:espace_out(Inst_name, Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec rd(tuple()) -> any().
rd(Pattern) when is_tuple(Pattern) ->
    rd(espace, Pattern).

-spec rd(atom(), tuple()) -> any().
rd(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_rd(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rdp" operation via the espace server.
%%
%% @end
%%--------------------------------------------------------------------
-spec rdp(tuple()) -> any().
rdp(Pattern) when is_tuple(Pattern) ->
    rdp(espace, Pattern).

-spec rdp(atom(), tuple()) -> any().
rdp(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_rdp(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Read and process an espace input file.
%% The file should be a valid Erlang terms file.
%%
%% @end
%%--------------------------------------------------------------------
-spec infile(atom() | binary() | [atom() | [any()] | char()]) -> 'ok'.
infile(File) ->
    infile(espace, File).

-spec infile(atom(), atom() | binary() | [atom() | [any()] | char()]) -> 'ok'.
infile(Inst_name, File) ->
    {ok, Terms} = file:consult(File),
    do_esp(Inst_name, Terms).


%%--------------------------------------------------------------------
%% @doc
%%
%% convert an instance name to longer prefixed name. This is used for
%% obtaining the instance specific server/table names.
%%
%% If the instance name is `espace', then the prefix is returned
%% without an instance name.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_to_name(atom(), atom()) -> atom().
inst_to_name(Prefix, Inst_name) ->
    case Inst_name  of
	espace ->
	    Prefix;
	_ ->
	    list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Inst_name))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Process espace input commands
%%
%% @end
%%--------------------------------------------------------------------
-spec do_esp(atom(), [{'eval', tuple()} | {'include', string()} | {'out', tuple()}]) -> 'ok'.
do_esp(_Inst_name, []) ->
    ok;

do_esp(Inst_name, [ {Cmd, Arg} | Rest]) ->
    case Cmd of
	eval ->
	    eval(Inst_name, Arg);
	out ->
	    out(Inst_name, Arg);
	include ->
	    infile(Inst_name, Arg)
    end,
    do_esp(Inst_name, Rest).
