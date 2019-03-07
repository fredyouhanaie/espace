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

-define(Test_file1, "test/testfile-1.esp").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

start_test() ->
    espace:start().

start_named_test() ->
    espace:start(aaa),
    espace:stop(aaa).

stop_test() ->
    application:ensure_all_started(espace),
    espace:stop().

eval_tuple_test() ->
    application:ensure_all_started(espace),
    espace:eval({erlang, system_time, []}).

eval_fun_test() ->
    application:ensure_all_started(espace),
    espace:eval({fun () -> erlang:system_time() end, []}).

eval_fun_str_test() ->
    application:ensure_all_started(espace),
    espace:eval({"fun () -> erlang:system_time() end.", []}).

worker_tuple_test() ->
    application:ensure_all_started(espace),
    espace:worker({erlang, system_time, []}).

worker_fun_test() ->
    application:ensure_all_started(espace),
    espace:worker({fun () -> erlang:system_time() end, []}).

worker_fun_str_test() ->
    application:ensure_all_started(espace),
    espace:worker({"fun () -> erlang:system_time() end.", []}).

out_test() ->
    application:ensure_all_started(espace),
    done = espace:out({test, 123, "ABC"}).

in_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    done = espace:out({Ref}),
    espace:in({Ref}).

rd_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    done = espace:out({Ref}),
    espace:rd({Ref}).

inp_match_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    done = espace:out({Ref}),
    espace:inp({Ref}).

inp_nomatch_test() ->
    application:ensure_all_started(espace),
    nomatch = espace:inp({erlang:make_ref()}).

rdp_match_test() ->
    application:ensure_all_started(espace),
    Ref = erlang:make_ref(),
    done = espace:out({Ref}),
    espace:rdp({Ref}).

rdp_nomatch_test() ->
    application:ensure_all_started(espace),
    nomatch = espace:rdp({erlang:make_ref()}).

adder1_eval_test() ->
    application:ensure_all_started(espace),
    espace:eval({?MODULE, test_add, []}),
    done = espace:out({add, 1, 2}),
    done = espace:out({add, 2, 3}),
    done = espace:out({add, 3, 4}),
    {[3], _} = espace:in({sum, 1, 2, '$3'}),
    {[5], _} = espace:in({sum, 2, 3, '$3'}),
    {[7], _} = espace:in({sum, 3, 4, '$3'}).

adder1_worker_test() ->
    application:ensure_all_started(espace),
    espace:worker({?MODULE, test_add, []}),
    done = espace:out({add, 1, 2}),
    done = espace:out({add, 2, 3}),
    done = espace:out({add, 3, 4}),
    {[3], _} = espace:in({sum, 1, 2, '$3'}),
    {[5], _} = espace:in({sum, 2, 3, '$3'}),
    {[7], _} = espace:in({sum, 3, 4, '$3'}).

infile_test() ->
    application:ensure_all_started(espace),
    espace:infile(?Test_file1),
    {[3], _} = espace:in({sum, 1, 2, '$3'}),
    {[7], _} = espace:in({sum, 3, 4, '$3'}).

%%--------------------------------------------------------------------
%% supporting functions for the tests
%%--------------------------------------------------------------------

test_add() ->
    {[A, B], _} = espace:in({add, '$1', '$2'}),
    done = espace:out({sum, A, B, A+B}),
    test_add().
