-module(adder1).

-export([start/0, test_add2/0, test_sums/0]).

-spec start() -> 'ok'.
start() ->
    espace:eval({adder1, test_add2, []}),
    espace:eval({adder1, test_sums, []}),
    espace:out({add, 1, 2}),
    espace:out({add, 2, 3}),
    espace:out({add, 3, 5}).

%% add two numbers
-spec test_add2() -> no_return().
test_add2() ->
    {[A, B], _} = espace:in({add, '$1', '$2'}),
    espace:out({sum, A, B, A+B}),
    test_add2().

%% take and print the sum results
-spec test_sums() -> no_return().
test_sums() ->
    {[X, Y, Sum], _} = espace:in({sum, '$1', '$2', '$3'}),
    io:format("~n~p + ~p = ~p.~n", [X, Y, Sum]),
    test_sums().
