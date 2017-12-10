-module(espace_test1).

-export([test_add1/2, test_add2/2]).

test_add1(A, B) ->
    io:format("test_add: ~p + ~p is ~p.~n", [A, B, A+B]).

test_add2(A, B) ->
    espace:out({sum, A, B, A+B}).
