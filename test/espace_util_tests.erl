%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the `espace_util' module.
%%% @end
%%% Created :  5 Jun 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_util_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

pterm_put() ->
    ok = espace_util:pterm_put(inst_put, hello, hello_world),
    hello_world == persistent_term:get({espace, inst_put, hello}).

pterm_erase_test_() ->
    {setup,
     fun () ->
             ok = espace_util:pterm_put(inst_erase, alpha, alpha_123),
             ok = espace_util:pterm_put(inst_erase, beta, beta_123)
     end,
     fun (_) -> ok end,
     [ {"erase inst keys 1", ?_assertEqual(lists:sort(espace_util:pterm_erase(inst_erase)),
                                           [{{espace, inst_erase, alpha}, alpha_123},
                                            {{espace, inst_erase, beta}, beta_123}]) },
       {"erase inst keys 2", ?_assertEqual(espace_util:pterm_erase(inst_erase), []) }
     ]}.

pterm_test_() ->
    {foreach,
     fun () -> espace_util:pterm_put(aaa, bbb, hello) end,
     fun (_) -> ok end,
     [ {"get item 1 (new)", ?_assertEqual(espace_util:pterm_get(aaa, bbb), espace_util:pterm_get(aaa, bbb)) },
       {"get item 2 (old)", ?_assertEqual(persistent_term:get(aaa, bbb), bbb) },
       {"put item 1", ?_assert(pterm_put())}
     ]}.

%%--------------------------------------------------------------------

inst_to_name_test_() ->
    [ { "any prefix, espace instance", ?_assertEqual(hello, espace_util:inst_to_name(hello, espace)) },
      { "any prefix, any instance", ?_assertEqual(hello_aaa, espace_util:inst_to_name(hello, aaa)) },
      { "espace prefix, any instance", ?_assertEqual(espace_aaa, espace_util:inst_to_name(espace, aaa)) }
    ].

%%--------------------------------------------------------------------

setup() ->
    espace:start(),
    logger:set_primary_config(#{level => error}).

cleanup(_) ->
    espace:stop().

eval_out_test_() ->
    {foreach, fun setup/0, fun cleanup/1,

     [ {"fun with no args",
	[ ?_assertEqual(done, espace_util:eval_out({five, fun () -> 2+3 end})),
	  ?_assertEqual({[], {five, 5}}, espace:in({five, 5})) ]},

       {"fun with args",
	[ ?_assertEqual(done, espace_util:eval_out({five, {fun (X, Y) -> X+Y end, [2, 3]}})),
	  ?_assertEqual({[], {five, 5}}, espace:in({five, 5})) ]},

       {"fun with incompatible fun/args pair",
	[ ?_assertEqual(done, espace_util:eval_out({five, {fun (X, Y) -> X+Y end, [2, 3, 4]}})),
	  ?_assertMatch({[{F, [2, 3, 4]}], {five, _}} when is_function(F, 2), espace:in({five, '$1'}))
	] }

     ]}.

%%--------------------------------------------------------------------

-define(Inst_name, aaa).

setup_named() ->
    espace:start(?Inst_name),
    logger:set_primary_config(#{level => error}).

cleanup_named(_) ->
    espace:stop(?Inst_name).

eval_out_named_test_() ->
    {foreach, fun setup_named/0, fun cleanup_named/1,

     [ {"fun with no args",
	[ ?_assertEqual(done, espace_util:eval_out(?Inst_name, {five, fun () -> 2+3 end})),
	  ?_assertEqual({[], {five, 5}}, espace:in(?Inst_name, {five, 5})) ]},

       {"fun with args",
	[ ?_assertEqual(done, espace_util:eval_out(?Inst_name, {five, {fun (X, Y) -> X+Y end, [2, 3]}})),
	  ?_assertEqual({[], {five, 5}}, espace:in(?Inst_name, {five, 5})) ]},

       {"fun with incompatible fun/args pair",
	[ ?_assertEqual(done, espace_util:eval_out(?Inst_name, {five, {fun (X, Y) -> X+Y end, [2, 3, 4]}})),
	  ?_assertMatch({[{F, [2, 3, 4]}], {five, _}} when is_function(F, 2), espace:in(?Inst_name, {five, '$1'}))
	] }
     ]
    }.

%%--------------------------------------------------------------------

opcounter_test_() ->
    [{"no counter ref pterm before start",
      ?_assertEqual(undefined,
                    espace_util:pterm_get(espace, opcounters)
                   )},

     {setup,
      fun () -> espace_util:opcount_new() end,
      fun (_) -> espace_util:pterm_erase(espace) end,
      [{"counter ref pterm exists after start",
        ?_assertNotEqual(undefined, espace_util:pterm_get(espace, opcounters) )},

       {"intial counts are zero",
        ?_assert(lists:all(fun (X) -> X==0 end,
                           maps:values(espace_util:opcount_counts())) )},

       {"increment, check counts",
        ?_assertEqual(incr_list([in, rd, rd, inp, inp, inp, eval]),
                      #{in=>1, rd=>2, inp=>3, rdp=>0, out=>0, eval=>1} )}
      ]},

     {"no counter ref pterm after stop",
      ?_assertEqual(undefined, espace_util:pterm_get(espace, opcounters))}
    ].

%% given a list of counter names, increment each one in turn, then
%% return the counts
incr_list([]) ->
    espace_util:opcount_counts();

incr_list([C|Rest]) ->
    espace_util:opcount_incr(C),
    incr_list(Rest).

%%--------------------------------------------------------------------
