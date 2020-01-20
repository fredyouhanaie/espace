%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Set of benchmarks for the various `espace' operations.
%%%
%%% @end
%%% Created : 11 Oct 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>

-module(bench_espace_1).

-export([ out_1/1, bench_out_1/2 ]).
-export([ out_1_named/1, bench_out_1_named/2 ]).

-export([ eval_1_nofun/1, bench_eval_1_nofun/2 ]).
-export([ eval_2_expr/1, bench_eval_2_expr/2 ]).

-export([ eval_1_named/1, bench_eval_1_named/2 ]).
-export([ eval_2_named/1, bench_eval_2_named/2 ]).

-export([ bench_start_stop_unnamed/2 ]).
-export([ bench_start_stop_named1/2 ]).
-export([ bench_start_stop_named2/2 ]).

-export([ bench_inst2name_1/2]).


%% simple `out' operation
%%
out_1(init) ->
    logger:set_primary_config(level, error),
    espace:start();
out_1({stop, _}) ->
    espace:stop().
bench_out_1(_, _) ->
    espace:out({abc, 123}).

%% `out' on a named instance
%%
out_1_named(init) ->
    logger:set_primary_config(level, error),
    espace:start(aaa);
out_1_named({stop, _}) ->
    espace:stop(aaa).
bench_out_1_named(_, _) ->
    espace:out(aaa, {abc, 123}).

%% simple `eval' with no function to evaluate, equivalent to `out_1'
%%
eval_1_nofun(init) ->
    logger:set_primary_config(level, error),
    espace:start();
eval_1_nofun({stop, _}) ->
    espace:stop().
bench_eval_1_nofun(_, _) ->
    espace:eval({abc, 123}).

%% eval with a function to be evaluated
%%
eval_2_expr(init) ->
    logger:set_primary_config(level, error),
    espace:start();
eval_2_expr({stop, _}) ->
    espace:stop().
bench_eval_2_expr(_, _) ->
    espace:eval({abc, fun () -> 2+2 end}).

%% simple (no fun) `eval' on a named instance
%%
eval_1_named(init) ->
    logger:set_primary_config(level, error),
    espace:start(aaa);
eval_1_named({stop, _}) ->
    espace:stop(aaa).
bench_eval_1_named(_, _) ->
    espace:eval(aaa, {abc, 123}).

%% `eval' with fun on a named instance
%%
eval_2_named(init) ->
    logger:set_primary_config(level, error),
    espace:start(aaa);
eval_2_named({stop, _}) ->
    espace:stop(aaa).
bench_eval_2_named(_, _) ->
    espace:eval(aaa, {abc, fun () -> 2+2 end}).

%% start/stop of the default instance, without any ops
%%
bench_start_stop_unnamed(_, _) ->
    logger:set_primary_config(level, error),
    espace:start(),
    espace:stop().

%% start/stop of the named default instance, without any ops
%%
bench_start_stop_named1(_, _) ->
    logger:set_primary_config(level, error),
    espace:start(espace),
    espace:stop(espace).

%% start/stop of a named instance, without any ops
%%
bench_start_stop_named2(_, _) ->
    logger:set_primary_config(level, error),
    espace:start(aaa),
    espace:stop(aaa).

%% `inst_to_name'
%%
bench_inst2name_1(_,_) ->
    espace_util:inst_to_name(aaa, bbb).
