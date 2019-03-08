%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2017, Fred Youhanaie
%%% @doc
%%% This module provides the client access to the user applications.
%%%
%%% In order to run any espace based applications the application
%%% needs to be started using one of the `start/0,1' functions.
%%%
%%% Multiple independent instances of `espace' can be active within
%%% the same node without interfering with each other. There can be up
%%% to one unnamed instance, and as many named instance as desired.
%%% Instance names are short atoms that are appended to the various
%%% server and table names.
%%%
%%% All client functions here have two variants, with and without
%%% instance name. If no instance name is supplied, or if the instance
%%% name is `espace', then the unnamed instance is assumed.
%%%
%%% @end
%%% Created : 10 Dec 2017 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace).

%% API
-export([eval/1, worker/1, in/1, inp/1, out/1, rd/1, rdp/1, infile/1, start/0, stop/0]).
-export([eval/2, worker/2, in/2, inp/2, out/2, rd/2, rdp/2, infile/2, start/1, stop/1]).
-export([inst_to_name/2]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Start a new unnamed instance of espace.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    start(espace).

%%--------------------------------------------------------------------
%% @doc Start a new named instance of espace.
%%
%% @end
%%--------------------------------------------------------------------
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
%% @doc Stop the unnamed instance of espace.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    stop(espace).

%%--------------------------------------------------------------------
%% @doc Stop a named instance of espace.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok | {error, term()}.
stop(Inst_name) ->
    application:stop(Inst_name).


%%--------------------------------------------------------------------
%% @doc Perform an `eval' operation via the unnamed espace server.
%%
%% See `eval/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval(tuple()) -> pid().
eval(MFA) when is_tuple(MFA) ->
    eval(espace, MFA).

%%--------------------------------------------------------------------
%% @doc Perform an `eval' operation via a named espace server.
%%
%% Currently `eval' behaves similar to `worker', however this will
%% change in the very near future. Please use the `worker/1,2'
%% functions instead of `eval/1,2'.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval(atom(), tuple()) -> pid().
eval(Inst_name, MFA) when is_tuple(MFA) ->
    tspool_srv:espace_eval(Inst_name, MFA).

%%--------------------------------------------------------------------
%% @doc
%% start a new worker process via the unnamed espace server.
%%
%% See `worker/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec worker(tuple()) -> pid().
worker(MFA) when is_tuple(MFA) ->
    worker(espace, MFA).

%%--------------------------------------------------------------------
%% @doc
%% start a new worker process via a named espace server.
%%
%% The function expects a single tuple as argument, which can have one
%% of three forms:
%%
%% <ul>
%%
%% <li>A `{Mod, Fun, Args}' triple, e.g. `{adder1, test_add2, []}'.</li>
%%
%% <li>A single fun, e.g. `{fun () -> adder1:test_add2() end}'.</li>
%%
%% <li>A string containing a fun, e.g. `"fun () -> adder1:test_add2()
%% end."'. This is mainly for use in espace input files.</li>
%%
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec worker(atom(), tuple()) -> pid().
worker(Inst_name, MFA) when is_tuple(MFA) ->
    tspool_srv:espace_worker(Inst_name, MFA).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `in' operation via the unnamed espace server.
%%
%% See `in/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec in(tuple()) -> {list(), tuple()}.
in(Pattern) ->
    in(espace, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `in' operation via a named espace server.
%%
%% For details of possible patterns see the match_spec in
%% `ets:match/2,3'. `Pattern' must be a tuple. It may consist of
%% expressions, or it may contain `$N' pattern variables, where N>0.
%%
%% If the pattern does not match a tuple in the tuple space, then the
%% call will block until such a pattern is added to the tuple
%% space.
%%
%% If a match is found, the tuple is returned as a pair `{List,
%% Tuple}', where `Tuple' is the whole tuple, and `List' is a,
%% possibly empty, list containing the matched `$N' pattern
%% variables. The items in the list will be ordered by the `$N'
%% variable numbers.
%%
%% For example the tuple `{add, 34, 88}' will match the pattern {add,
%% '$2', '$1'}, and the returned result will be '{[88, 34], {add, 34,
%% 88}}'. Note the order of the numbers in the list.
%%
%% The matched tuple will be removed from the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec in(atom(), tuple()) -> {list(), tuple()}.
in(Inst_name, Pattern) ->
    tspool_srv:espace_in(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `inp' operation via the unnamed espace server.
%%
%% See `inp/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec inp(tuple()) -> nomatch | {list(), tuple()}.
inp(Pattern) when is_tuple(Pattern) ->
    inp(espace, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `inp' operation via a named espace server.
%%
%% The `inp/1,2' functions are the non-blocking versions of
%% `in/1,2`. If a match is not found in the first instance, then the
%% atom `nomatch' is returned. If a match is found, then it will be
%% removed from the tuple space and returned as a `{List, Tuple}'
%% pair.
%%
%% See `in/1,2' for details and examples of the match patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec inp(atom(), tuple()) -> nomatch | {list(), tuple()}.
inp(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_inp(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `out' operation via the unnamed espace server.
%%
%% See `out/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec out(tuple()) -> done.
out(Tuple) when is_tuple(Tuple) ->
    out(espace, Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Perform an `out' operation via a named espace server.
%%
%% The `Tuple' supplied as argument will be added to the tuple
%% space. Duplicate tuples are allowed in the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec out(atom(), tuple()) -> done.
out(Inst_name, Tuple) when is_tuple(Tuple) ->
    tspool_srv:espace_out(Inst_name, Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rd' operation via the unnamed espace server.
%%
%% See `rd/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec rd(tuple()) -> {list(), tuple()}.
rd(Pattern) when is_tuple(Pattern) ->
    rd(espace, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rd' operation via a named espace server.
%%
%% The `rd/1,2' functions behave in the same manner as `in/1,2' except
%% that when a matching tuple is found, it will not be removed from
%% the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec rd(atom(), tuple()) -> {list(), tuple()}.
rd(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_rd(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rdp' operation via the unnamed espace server.
%%
%% See `rdp/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec rdp(tuple()) -> nomatch | {list(), tuple()}.
rdp(Pattern) when is_tuple(Pattern) ->
    rdp(espace, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rdp' operation via a named espace server.
%%
%% The functions `rdp/1,2' are the non-blocking versions of
%% `rd/1,2'. That is, if a match is not found, then `nomatch' is
%% returned, and if a match is found the matching tuple will be
%% return, but it will not be removed from the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec rdp(atom(), tuple()) -> nomatch | {list(), tuple()}.
rdp(Inst_name, Pattern) when is_tuple(Pattern) ->
    tspool_srv:espace_rdp(Inst_name, Pattern).

%%--------------------------------------------------------------------
%% @doc
%% Read and process an espace input file via the unnamed server.
%%
%% See infile/2 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec infile(file:name_all()) -> ok.
infile(File) ->
    infile(espace, File).

%%--------------------------------------------------------------------
%% @doc
%% Read and process an espace input file via a named server.
%%
%% The file should be a valid Erlang terms file.
%%
%% @end
%%--------------------------------------------------------------------
-spec infile(atom(), file:name_all()) -> ok.
infile(Inst_name, File) ->
    {ok, Terms} = file:consult(File),
    do_esp(Inst_name, Terms).

%%--------------------------------------------------------------------
%% @doc
%% Convert an instance name to longer prefixed name.
%%
%% This is used for obtaining the instance specific server/table
%% names. For example `inst_to_name(espace_sup, aaa)' will return
%% `espace_sup_aaa'.
%%
%% If the instance name is `espace', then the prefix is returned
%% without an instance name suffix. For example
%% `inst_to_name(espace_sup, espace)' will return `espace_sup'.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_to_name(atom(), atom()) -> atom().
inst_to_name(Prefix, espace) ->
    Prefix;
inst_to_name(Prefix, Inst_name) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Inst_name)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Process espace input commands
%%
%% @end
%%--------------------------------------------------------------------
-spec do_esp(atom(), [{eval, tuple()} | {worker, tuple()} | {include, string()} | {out, tuple()}]) -> ok.
do_esp(_Inst_name, []) ->
    ok;

do_esp(Inst_name, [ {Cmd, Arg} | Rest]) ->
    case Cmd of
	eval ->
	    eval(Inst_name, Arg);
	worker ->
	    worker(Inst_name, Arg);
	out ->
	    out(Inst_name, Arg);
	include ->
	    infile(Inst_name, Arg)
    end,
    do_esp(Inst_name, Rest).
