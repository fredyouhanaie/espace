%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2018, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the espace module
%%% @end
%%% Created :  1 Jun 2018 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_tests).

-export([test_add/0]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

eval_tuple_test() ->
    application:ensure_all_started(espace),
    espace:eval({erlang, system_time, []}).

eval_fun_test() ->
    application:ensure_all_started(espace),
    espace:eval({"fun () -> erlang:system_time() end.", []}).

out_test() ->
    application:ensure_all_started(espace),
    espace:out({test, 123, "ABC"}).

in_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    espace:out({Ref}),
    espace:in({Ref}).

rd_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    espace:out({Ref}),
    espace:rd({Ref}).

inp_test() ->
    application:ensure_all_started(espace),
    nomatch = espace:inp({erlang:make_ref()}).

rdp_test() ->
    application:ensure_all_started(espace),
    nomatch = espace:rdp({erlang:make_ref()}).

adder1_test() ->
    application:ensure_all_started(espace),
    espace:eval({adder1, test_add2, []}),
    espace:out({add, 1, 2}),
    espace:out({add, 2, 3}),
    espace:out({add, 3, 4}),
    {[3], _} = espace:in({sum, 1, 2, '$3'}),
    {[5], _} = espace:in({sum, 2, 3, '$3'}),
    {[7], _} = espace:in({sum, 3, 4, '$3'}).

%%--------------------------------------------------------------------
%% supporting functions for the tests
%%--------------------------------------------------------------------

test_add() ->
    {[A, B], _} = espace:in({add, '$1', '$2'}),
    espace:out({sum, A, B, A+B}),
    test_add().
