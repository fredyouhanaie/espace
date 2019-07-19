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

inst_to_name_1_test() ->
    hello = espace_util:inst_to_name(hello, espace).

inst_to_name_2_test() ->
    hello_aaa = espace_util:inst_to_name(hello, aaa).

inst_to_name_3_test() ->
    espace_aaa = espace_util:inst_to_name(espace, aaa).

eval_out_1_test() ->
    application:ensure_all_started(espace),
    done = espace_util:eval_out({five, fun () -> 2+3 end}),
    {[], {five, 5}} = espace:in({five, 5}).

eval_out_2_test() ->
    application:ensure_all_started(espace),
    done = espace_util:eval_out({five, {fun (X, Y) -> X+Y end, [2, 3]}}),
    {[], {five, 5}} = espace:in({five, 5}).

eval_out_3_test() ->
    espace:start(aaa),
    done = espace_util:eval_out(aaa, {five, {fun (X, Y) -> X+Y end, [2, 3]}}),
    {[], {five, 5}} = espace:in(aaa, {five, 5}).

eval_out_4_test() ->
    application:ensure_all_started(espace),
    done = espace_util:eval_out({five, {fun (X, Y) -> X+Y end, [2, 3, 4]}}),
    {[{F, L}], {five, _}} = espace:in({five, '$1'}),
    ?assert(is_function(F, 2)),
    L = [2, 3, 4].

start_test() ->
    espace:start().

start_named_test() ->
    espace:start(aaa),
    espace:stop(aaa).

stop_test() ->
    application:ensure_all_started(espace),
    espace:stop().

eval_simple_test() ->
    application:ensure_all_started(espace),
    Pid = espace:eval({five, fun () -> 2+3 end}),
    ?assert(is_pid(Pid)),
    {[], {five, 5}} =  espace:in({five, 5}).

eval_tuple_test() ->
    application:ensure_all_started(espace),
    Pid = espace:eval({five, {fun (X,Y) -> X+Y end, [2, 3]}}),
    ?assert(is_pid(Pid)),
    {[], {five, 5}} =  espace:in({five, 5}).


worker_tuple_test() ->
    application:ensure_all_started(espace),
    Pid = espace:worker({erlang, system_time, []}),
    ?assert(is_pid(Pid)).

worker_fun_test() ->
    application:ensure_all_started(espace),
    Pid = espace:worker({fun () -> erlang:system_time() end, []}),
    ?assert(is_pid(Pid)).

worker_fun_str_test() ->
    application:ensure_all_started(espace),
    Pid = espace:worker({"fun () -> erlang:system_time() end.", []}),
    ?assert(is_pid(Pid)).

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
    espace:eval({fun () -> ?MODULE:test_add() end}),
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

etsmgr_tspace_1_test() ->
    application:ensure_all_started(espace),

    Tuple_1 = {hello, 1},
    espace:out(Tuple_1),
    erlang:exit(whereis(etsmgr_srv_espace), kill),
    timer:sleep(10),
    {[], Tuple_1} = espace:in(Tuple_1),

    application:stop(espace).

etsmgr_tspace_2_test() ->
    application:ensure_all_started(espace),

    Tuple_2 = {goodbye, 2},
    espace:out(Tuple_2),
    erlang:exit(whereis(tspace_srv), kill),
    timer:sleep(1000),
    {[], Tuple_2} = espace:in(Tuple_2),

    application:stop(espace).

%%--------------------------------------------------------------------
%% supporting functions for the tests
%%--------------------------------------------------------------------

test_add() ->
    {[A, B], _} = espace:in({add, '$1', '$2'}),
    done = espace:out({sum, A, B, A+B}),
    test_add().
