%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%% run the EUnit tests for the `espace_observer' module.
%%% @end
%%% Created : 28 Mar 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_observer_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% The tests
%%--------------------------------------------------------------------

header(Header_maplist) ->
    [maps:get(title, Header) || Header <- Header_maplist].

%%--------------------------------------------------------------------

-define(Header, [inst_name, tuples, waiting,
                 "in", "rd", "inp", "rdp", "out", "eval"]).

callbacks_1_test_() ->
    [ {"empty attributes",
       ?_assertEqual({[], 42}, espace_observer:attributes(42) )},
      {"sheet headers",
       ?_assertEqual(?Header, header(espace_observer:sheet_header()) )},
      {"empty body (no espace)",
       ?_assertEqual({[], 42}, espace_observer:sheet_body(42) )}
    ].

%%--------------------------------------------------------------------

counts({Counts, _}) ->
    lists:sort(Counts).

setup() ->
    espace:start(aaa),
    espace:start().

cleanup(_) ->
    espace:stop(),
    espace:stop(aaa).

-define(Counts_0,
        [ [ aaa,    0, 0, 0, 0, 0, 0, 0, 0 ],
          [ espace, 0, 0, 0, 0, 0, 0, 0, 0 ]
        ]).

callbacks_2_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     [ {"empty attributes",
        ?_assertEqual({[], 42}, espace_observer:attributes(42) )},
       {"sheet headers",
        ?_assertEqual(?Header, header(espace_observer:sheet_header()) )},
       {"empty body (two idle instances",
        ?_assertEqual({?Counts_0, 42}, {counts(espace_observer:sheet_body(42)), 42} )}
     ]}.

%%--------------------------------------------------------------------
