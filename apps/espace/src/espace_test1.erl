-module(espace_test1).

-export([start/0, test_add2/0, test_sums/0]).

start() ->
    espace_cli:eval({espace_test1, test_add2, []}),
    espace_cli:eval({espace_test1, test_sums, []}),
    espace_cli:out({add, 1, 2}),
    espace_cli:out({add, 2, 3}),
    espace_cli:out({add, 3, 5}).

%% add two numbers
test_add2() ->
    {[A, B], _} = espace_cli:in({add, '$1', '$2'}),
    espace_cli:out({sum, A, B, A+B}),
    test_add2().

%% take and print the sum results
test_sums() ->
    {[X, Y, Sum], _} = espace_cli:in({sum, '$1', '$2', '$3'}),
    io:format("~n~p + ~p = ~p.~n", [X, Y, Sum]),
    test_sums().
