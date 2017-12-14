%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_cli).

%% API
-export([eval/1, in/1, inp/1, out/1, rd/1, rdp/1, infile/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Perform an "eval" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
eval(MFA) ->
    wkpool_srv:espace_eval(MFA),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Perform an "in" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
in(Pattern) ->
    tspool_srv:espace_in(Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "inp" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
inp(Pattern) ->
    tspool_srv:espace_inp(Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an "out" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
out(Tuple) ->
    tspool_srv:espace_out(Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rd" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
rd(Pattern) ->
    tspool_srv:espace_rd(Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a "rdp" operation via the espace server.
%% @spec
%% @end
%%--------------------------------------------------------------------
rdp(Pattern) ->
    tspool_srv:espace_rdp(Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Read and process an espace input file.
%% The file should be a valid Erlang terms file.
%% @spec
%% @end
%%--------------------------------------------------------------------
infile(File) ->
    {ok, Terms} = file:consult(File),
    do_esp(Terms).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Process espace input commands
%% @spec
%% @end
%%--------------------------------------------------------------------
do_esp([]) ->
    ok;

do_esp([ {Cmd,Arg} | Rest]) ->
    case Cmd of
	eval ->
	    eval(Arg);
	out ->
	    out(Arg);
	include ->
	    infile(Arg)
    end,
    do_esp(Rest).
