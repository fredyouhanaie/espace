% -*- indent-tabs-mode:nil; -*-
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

%% setup/cleanup funcions used by the rest of the tests

setup() ->
    espace:start(),
    logger:set_primary_config(#{level => error}).

cleanup(_) ->
    espace:stop().

%%--------------------------------------------------------------------

start_stop_test_() ->
    {inorder,

     [ {"unnamed start", ?_assertEqual(ok, espace:start())},
       {"unnamed start again", ?_assertEqual({error, {already_started, espace}}, espace:start())},

       {"unnamed stop", ?_assertEqual(ok, espace:stop())},
       {"unnamed stop again", ?_assertEqual({error, {not_started, espace}}, espace:stop())},

       {"named start", ?_assertEqual(ok, espace:start(aaa))},
       {"named start again", ?_assertEqual({error, {already_started, aaa}}, espace:start(aaa))},

       {"named stop", ?_assertEqual(ok, espace:stop(aaa))},
       {"named stop again", ?_assertEqual({error, {not_started, aaa}}, espace:stop(aaa))}

     ]}.

%%--------------------------------------------------------------------

eval_test_() ->
    {setup, fun setup/0, fun cleanup/1,

     [ {"eval simple fun",
        [ {"run eval", ?_assertMatch(Pid when is_pid(Pid), espace:eval({five, fun () -> 2+3 end}))},
          {"check eval output", ?_assertEqual({[], {five, 5}}, espace:in({five, 5}))}
        ]},

       {"eval fun tuple",
        [ {"run eval", ?_assertMatch(Pid when is_pid(Pid), espace:eval({five, {fun (X,Y) -> X+Y end, [2, 3]}}))},
          {"check eval output", ?_assertEqual({[], {five, 5}}, espace:in({five, 5}))}
        ]}

     ]}.
      
%%--------------------------------------------------------------------

worker_test_() ->
    [
     {setup, fun setup/0, fun cleanup/1,

      [ {"worker as tuple",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({erlang, system_time, []}))},

        {"worker as fun tuple with args",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({fun () -> erlang:system_time() end, []}))},

        {"worker as fun no args",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({fun () -> erlang:system_time() end}))},

        {"worker as fun expr",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({fun erlang:system_time/0}))},

        {"worker as fun string",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({"fun () -> erlang:system_time() end.", []}))}

      ]}
    ].

%%--------------------------------------------------------------------

out_ref() ->
    Ref = erlang:make_ref(),
    done = espace:out({Ref}),
    Ref.

tuple_ops_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [ {"out",
        ?_assertEqual(done, espace:out({test, 123, "ABC"}))},

       {"in with match",
        ?_assertMatch({[], {R}} when is_reference(R), espace:in({out_ref()}))},

       {"in no match",
        {inorder, [?_assertMatch({ok, _}, timer:apply_after(50, espace, stop, [])),
                   ?_assertEqual(quit, espace:in({erlang:make_ref()}))]}},

       {"rd with match",
        ?_assertMatch({[], {R}} when is_reference(R), espace:rd({out_ref()}))},

       {"rd no match",
        {inorder, [?_assertMatch({ok, _}, timer:apply_after(50, espace, stop, [])),
                   ?_assertEqual(quit, espace:rd({erlang:make_ref()}))]}},

       {"inp with match",
        ?_assertMatch({[], {R}} when is_reference(R), espace:inp({out_ref()}))},

       {"inp no match",
        ?_assertEqual(nomatch, espace:inp({erlang:make_ref()}))},

       {"rdp with match",
        ?_assertMatch({[], {R}} when is_reference(R), espace:rdp({out_ref()}))},

       {"rdp no match",
        ?_assertEqual(nomatch, espace:rdp({erlang:make_ref()}))}

     ]}.

%%--------------------------------------------------------------------

adder1_eval_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     {inorder,
      [ {"start adder via eval/1",
         ?_assertMatch(Pid when is_pid(Pid), espace:eval({fun () -> ?MODULE:test_add() end}))},

        {"out some tuples",
         [?_assertEqual(done, espace:out({add, 1, 2})),
          ?_assertEqual(done, espace:out({add, 2, 3})),
          ?_assertEqual(done, espace:out({add, 3, 4}))]},

        {"check the results",
         [?_assertMatch({[3], _}, espace:in({sum, 1, 2, '$3'})),
          ?_assertMatch({[5], _}, espace:in({sum, 2, 3, '$3'})),
          ?_assertMatch({[7], _}, espace:in({sum, 3, 4, '$3'}))]}

      ]}}.

adder1_worker_test_() ->
    {setup, fun setup/0, fun cleanup/1,

     {inorder,
      [ {"start adder via worker/1",
         ?_assertMatch(Pid when is_pid(Pid), espace:worker({?MODULE, test_add, []}))},

        {"out some tuples",
         [?_assertEqual(done, espace:out({add, 1, 2})),
          ?_assertEqual(done, espace:out({add, 2, 3})),
          ?_assertEqual(done, espace:out({add, 3, 4}))]},

        {"check the results",
         [?_assertMatch({[3], _}, espace:in({sum, 1, 2, '$3'})),
          ?_assertMatch({[5], _}, espace:in({sum, 2, 3, '$3'})),
          ?_assertMatch({[7], _}, espace:in({sum, 3, 4, '$3'}))]}

      ]}}.

%%--------------------------------------------------------------------

infile_test_() ->
    {setup, fun setup/0, fun cleanup/1,

     {inorder,
      [ {"process the file",
         ?_assertEqual(ok, espace:infile(?Test_file1))},

        {"check the results",
         [?_assertMatch({[3], _}, espace:in({sum, 1, 2, '$3'})),
          ?_assertMatch({[7], _}, espace:in({sum, 3, 4, '$3'}))
         ]}

      ]}}.

%%--------------------------------------------------------------------

etsmgr_recover() ->
    Tables1 = lists:sort( maps:keys(etsmgr:info(espace)) ),
    erlang:exit(whereis(etsmgr_srv_espace), kill),
    timer:sleep(1010),
    Tables2 = lists:sort( maps:keys(etsmgr:info(espace)) ),
    lists:prefix(Tables1, Tables2).

etsmgr_test_() ->
    {foreach, fun setup/0, fun cleanup/1,

     [ {"etsmgr kill/recover",
        ?_assert(etsmgr_recover())},

       {"etsmgr kill/restart",
        {inorder,
         [ {"drop a control tuple",
            ?_assertEqual(done, espace:out({hello, 1}))},

           {"find/kill the etsmgr server",
            ?_assert(erlang:exit(whereis(etsmgr_srv_espace), kill))},

           {"wait a bit",
            ?_assertEqual(ok, timer:sleep(10))},

           {"ensure the tuple still present",
            ?_assertMatch({[], {hello, 1}}, espace:in({hello, 1}))}

         ]}},

       {"tspace kill/restart",
        [ {"drop a control tuple",
           ?_assertEqual(done, espace:out({goodbye, 2}))},

          {"find/kill the tspace server",
           ?_assert(erlang:exit(whereis(espace_tspace_srv), kill))},

          {"wait a bit",
           ?_assertEqual(ok, timer:sleep(10))},

          {"ensure the tuple still present",
           ?_assertMatch({[], {goodbye, 2}}, espace:in({goodbye, 2}))}
        ]}

     ]}.

tspatt_recovery_test_() ->
    {setup, fun setup/0, fun cleanup/1,

     [ {"launch an eval to increment count",
        ?_assertMatch(Pid when is_pid(Pid),
                               espace:eval({count,
                                            fun () ->
                                                    {[Count], _} = espace:in({count, '$1'}),
                                                    Count+1
                                            end}))},

       {"find/kill tspatt server",
        ?_assert(erlang:exit(whereis(espace_tspatt_srv), kill))},

       {"wait for recovery",
        ?_assertEqual(ok, timer:sleep(10))},

       {"drop the count into TS",
        ?_assertEqual(done, espace:out({count, 1}))},

       {"check eval has recovered",
        ?_assertEqual({[], {count, 2}}, espace:in({count, 2}))}

     ]}.

%%--------------------------------------------------------------------
%% supporting functions for the tests
%%--------------------------------------------------------------------

test_add() ->
    case espace:in({add, '$1', '$2'}) of
        quit ->
            ok;
        {[A, B], _} ->
            done = espace:out({sum, A, B, A+B}),
            test_add()
    end.
