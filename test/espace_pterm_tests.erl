%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the `espace_pterm' module.
%%% @end
%%% Created : 26 Sep 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_pterm_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

pterm_put() ->
    ok = espace_pterm:put(inst_put, hello, hello_world),
    hello_world == persistent_term:get({espace, inst_put, hello}).

pterm_erase_test_() ->
    {setup,
     fun () ->
             ok = espace_pterm:put(inst_erase, alpha, alpha_123),
             ok = espace_pterm:put(inst_erase, beta, beta_123)
     end,
     fun (_) -> ok end,
     [ {"erase inst keys 1", ?_assertEqual(lists:sort(espace_pterm:erase(inst_erase)),
                                           [{{espace, inst_erase, alpha}, alpha_123},
                                            {{espace, inst_erase, beta}, beta_123}]) },
       {"erase inst keys 2", ?_assertEqual(espace_pterm:erase(inst_erase), []) }
     ]}.

pterm_test_() ->
    {foreach,
     fun () -> espace_pterm:put(aaa, bbb, hello) end,
     fun (_) -> ok end,
     [ {"get item 1 (new)", ?_assertEqual(espace_pterm:get(aaa, bbb), espace_pterm:get(aaa, bbb)) },
       {"get item 2 (old)", ?_assertEqual(persistent_term:get(aaa, bbb), bbb) },
       {"put item 1", ?_assert(pterm_put())}
     ]}.

%%--------------------------------------------------------------------
