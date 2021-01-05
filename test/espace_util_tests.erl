% -*- indent-tabs-mode:nil; -*-
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
