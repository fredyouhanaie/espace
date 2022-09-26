%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2021, Fred Youhanaie
%%% @doc
%%%
%%% run the EUnit tests for the `espace_opcount' module.
%%%
%%% @end
%%% Created :  5 Jan 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_opcount_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

opcounter_test_() ->
    [{"no counter ref pterm before start",
      ?_assertEqual(undefined,
                    espace_pterm:get(espace, opcounters)
                   )},

     {setup,
      fun () -> espace_opcount:new() end,
      fun (_) -> espace_pterm:erase(espace) end,
      [{"counter ref pterm exists after start",
        ?_assertNotEqual(undefined, espace_pterm:get(espace, opcounters) )},

       {"intial counts are zero",
        ?_assertEqual([0, 0, 0, 0, 0, 0],
                      maps:values(espace_opcount:counts()) )},

       {"increment, check counts",
        ?_assertEqual(incr_list([in, rd, rd, inp, inp, inp, eval]),
                      #{in=>1, rd=>2, inp=>3, rdp=>0, out=>0, eval=>1} )},

       {"reset the previous counts",
        ?_assertEqual([0,0,0,0,0,0],
                      maps:values(begin
                                      espace_opcount:reset(),
                                      espace_opcount:counts()
                                  end) )}

      ]},

     {"no counter ref pterm after stop",
      ?_assertEqual(undefined, espace_pterm:get(espace, opcounters))}
    ].

%% given a list of counter names, increment each one in turn, then
%% return the counts
incr_list([]) ->
    espace_opcount:counts();

incr_list([C|Rest]) ->
    espace_opcount:incr(C),
    incr_list(Rest).

%%--------------------------------------------------------------------
