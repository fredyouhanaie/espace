% -*- indent-tabs-mode:nil; -*-
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
start(Inst_name) when is_atom(Inst_name) ->
    %% retrieve the main app resource file
    {ok, [{application, espace, App_0}]} = file:consult(code:where_is_file("espace.app")),

    %% add the instance name suffix to the list of application servers
    {registered, Server_names_0} = lists:keyfind(registered, 1, App_0),
    Server_names_1 = lists:map(
                       fun (S) -> espace_util:inst_to_name(S, Inst_name) end,
                       Server_names_0),
    App_1 = lists:keyreplace(registered, 1, App_0, {registered, Server_names_1}),

    %% pass the instance name to the application as arg
    App_2 = lists:keyreplace(mod, 1, App_1, {mod, {espace_app, Inst_name}}),

    application:load({application, Inst_name, App_2}),
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
eval(Tuple) when is_tuple(Tuple) ->
    eval(espace, Tuple).

%%--------------------------------------------------------------------
%% @doc Perform an `eval' operation via a named espace server.
%%
%% The input Tuple is evaluated in a worker process and the result is
%% sent to the tuple space using `out' operation.
%%
%% The function returns the pid of the worker process.
%%
%% The elements of the output tuple correspond to those of
%% `Tuple'. If any of the elements of `Tuple' match the function
%% pattern, then the corresponding output element will be the value of
%% the function.
%%
%% The following patterns will trigger the evaluation of the second
%% element of the tuple:
%%
%% <ol>
%%
%% <li>Anonymous function, e.g.
%% <code>{aa, fun () -> 2+3 end</code>, zz}.
%% </li>
%%
%% <li>Tuple with function and args, e.g.
%% <code>{aa, {fun (X,Y) -> X+Y end, [2, 3]}, zz}</code></li>
%%
%% </ol>
%%
%% Both examples above will produce `{aa, 5, zz}'
%%
%% In the first example we have a `fun' expression of arity zero. In
%% the second, we have a pair (tuple) of a `fun' expression and a
%% `list', and the arity of the `fun' matches the length of the list.
%%
%% Any other pattern will move the element to the output tuple
%% untouched.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval(atom(), tuple()) -> pid().
eval(Inst_name, Tuple) when is_tuple(Tuple) ->
    {ok, Pid} = run_child(espace_util, eval_out, [Inst_name, Tuple]),
    Pid.


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
worker(Inst_name, {M, F, A}) ->
    logger:info("~p/worker: run_child M=~p, F=~p, A=~p.", [Inst_name, M, F, A]),
    {ok, Pid} = run_child(M, F, A),
    Pid;

worker(Inst_name, {Fun, Args}) ->
    logger:info("~p/worker: run_child, Fun=~p, Args=~p.", [Inst_name, Fun, Args]),
    {ok, Pid} = run_child(Fun, Args),
    Pid.


%%--------------------------------------------------------------------
%% @doc
%% Perform an `in' operation via the unnamed espace server.
%%
%% See `in/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec in(tuple()) -> {list(), tuple()} | quit.
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
%% call will block until such a pattern is added to the tuple space.
%% If the espace server terminates while the client is in blocking
%% state the atom `quit' will be returned.
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
-spec in(atom(), tuple()) -> {list(), tuple()} | quit.
in(Inst_name, Pattern) ->
    espace_op(Inst_name, in, Pattern).

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
    espace_op(Inst_name, inp, Pattern).

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
    espace_tspace_srv:add_tuple(Inst_name, Tuple).

%%--------------------------------------------------------------------
%% @doc
%% Perform a `rd' operation via the unnamed espace server.
%%
%% See `rd/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec rd(tuple()) -> {list(), tuple()} | quit.
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
-spec rd(atom(), tuple()) -> {list(), tuple()} | quit.
rd(Inst_name, Pattern) when is_tuple(Pattern) ->
    espace_op(Inst_name, rd, Pattern).

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
    espace_op(Inst_name, rdp, Pattern).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Process espace input commands
%%
%% @end
%%--------------------------------------------------------------------
-spec do_esp(atom(), [{worker, tuple()} | {include, string()} | {out, tuple()}]) -> ok.
do_esp(_Inst_name, []) ->
    ok;

do_esp(Inst_name, [ {Cmd, Arg} | Rest]) ->
    case Cmd of
        worker ->
            worker(Inst_name, Arg);
        out ->
            out(Inst_name, Arg);
        include ->
            infile(Inst_name, Arg)
    end,
    do_esp(Inst_name, Rest).


%%--------------------------------------------------------------------
%% @doc Spawn a child process given a `Module'/`Function'/`Args' triple.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_child(atom(), atom(), list()) -> {ok, pid()}.
run_child(M, F, A) ->
    logger:info("run_child M=~p, F=~p, A=~p.", [M, F, A]),
    {ok, spawn(M, F, A)}.


%%--------------------------------------------------------------------
%% @doc Run a child process given a function, or a string
%% representation of a function.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_child(string() | function(), list()) -> {ok, pid()}.
run_child(Fun, Args) when is_function(Fun) ->
    logger:info("run_child Fun=~p, Args=~p.", [Fun, Args]),
    {ok, spawn(erlang, apply, [Fun, Args])};

run_child(Fun, Args) ->
    logger:info("run_child Fun=~p, Args=~p.", [Fun, Args]),
    {ok, Tokens, _} = erl_scan:string(Fun),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, F, _} = erl_eval:exprs(Parsed, []),
    {ok, spawn(erlang, apply, [F, Args])}.


%%--------------------------------------------------------------------
%% @doc Perform one of the input ops, in/rd/inp/rdp.
%%
%% If a match is found we return the matched data.
%%
%% If a match is not found we expect to the `{nomatch, Cli_Ref}'
%% return value from the `tspace' server. For the blocking operations,
%% `in' and `rd', in which case we will wait indefinitely for the
%% `Cli_ref' message. Once we receive the message we try the operation
%% again.
%%
%% If the client is blocking on `rd' or `in' and the espace server is
%% terminating, a `quit' message will be received from the server, and
%% the atom `quit' will be returned to the caller.
%%
%% For the non-blocking operations, `inp' and `rdp', we just return
%% `nomatch' to the client.
%%
%% @end
%%--------------------------------------------------------------------
-spec espace_op(atom(), in|rd|inp|rdp, tuple()) -> nomatch | {list(), tuple()} | quit.
espace_op(Inst_name, Espace_op, Pattern) ->
    case espace_tspace_srv:get_tuple(Inst_name, Espace_op, Pattern) of
        {nomatch} ->
            nomatch;
        {nomatch, Cli_ref} ->
            receive
                {Cli_ref, quit} ->
                    quit;
                {Cli_ref, retry} ->
                    espace_op(Inst_name, Espace_op, Pattern)
            end;
        {match, {Fields, Tuple}} ->
            {Fields, Tuple}
    end.
