%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% A set of utility function to support the rest of the espace modules.
%%%
%%% @end
%%% Created : 11 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_util).

%% API
-export([eval_out/1, eval_out/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Conditionally evaluate a tuple and `out' the result to the
%% unnamed instance.
%%
%% See `eval_out/2' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval_out(tuple()) -> done.
eval_out(Tuple_in) ->
    eval_out(espace, Tuple_in).

%%--------------------------------------------------------------------
%% @doc Conditionally evaluate a tuple and `out' the result to a named
%% instance.
%%
%% The elements of the output tuple correspond to those of
%% `Tuple_in'. If any of the elements of `Tuple_in' are recognized as
%% function, then the corresponding output element will be the value
%% of the function.
%%
%% Two types of patterns are recognized as functions and are
%% evaluated.  A normal function expression of arity zero, `fun () ->
%% expr end'. And, a tuple with two elements, a function expresion of
%% arity `N' and a list of length `N', `N' can be zero.
%%
%% Any other pattern will move the element to the output tuple
%% untouched.
%%
%% @end
%%--------------------------------------------------------------------
-spec eval_out(atom(), tuple()) -> done.
eval_out(Inst_name, Tuple_in) ->
    List_in = erlang:tuple_to_list(Tuple_in),
    List_out = lists:map(fun (X) -> do_eval(X) end, List_in),
    Tuple_out = erlang:list_to_tuple(List_out),
    espace:out(Inst_name, Tuple_out).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc check and evaluate a term, if it is a function.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_eval(term()) -> term().
do_eval(Fun) when is_function(Fun, 0) ->
    erlang:apply(Fun, []);
do_eval({Fun, Args}) when is_list(Args) andalso is_function(Fun, length(Args)) ->
    erlang:apply(Fun, Args);
do_eval(X) ->
    X.

