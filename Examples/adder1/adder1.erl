-module(adder1).

-export([test_add2/1, test_sums/1]).
-export([start/0, stop/0]).
-export([start/1, stop/1]).

-spec start() -> 'ok'.
start() ->
    start(espace).

-spec start(atom()) -> ok.
start(Inst_name) ->
    espace:start(Inst_name),
    espace:eval(Inst_name, {adder1, test_add2, [Inst_name]}),
    espace:eval(Inst_name, {adder1, test_sums, [Inst_name]}),
    espace:out(Inst_name, {add, 1, 2}),
    espace:out(Inst_name, {add, 2, 3}),
    espace:out(Inst_name, {add, 3, 5}).

-spec stop() -> 'ok'.
stop() ->
    stop(espace).

-spec stop(atom()) -> ok.
stop(Inst_name) ->
    espace:stop(Inst_name).

%% add two numbers
-spec test_add2(atom()) -> no_return().
test_add2(Inst_name) ->
    {[A, B], _} = espace:in(Inst_name, {add, '$1', '$2'}),
    espace:out(Inst_name, {sum, A, B, A+B}),
    test_add2(Inst_name).

%% take and print the sum results
-spec test_sums(atom()) -> no_return().
test_sums(Inst_name) ->
    {[X, Y, Sum], _} = espace:in(Inst_name, {sum, '$1', '$2', '$3'}),
    io:format("~n~p: ~p + ~p = ~p.~n", [Inst_name, X, Y, Sum]),
    test_sums(Inst_name).
