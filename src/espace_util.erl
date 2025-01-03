%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2019, Fred Youhanaie
%%% @doc
%%%
%%% A set of utility functions to support the rest of the espace
%%% modules.
%%%
%%% @end
%%% Created : 11 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_util).

%% API
-export([eval_out/1, eval_out/2]).
-export([inst_to_name/2, wait4etsmgr/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Convert an instance name to longer prefixed name.
%%
%% This is used for obtaining the instance specific server/table
%% names. For example `inst_to_name(espace_sup, aaa)' will return
%% `espace_sup_aaa'.
%%
%% If the instance name is `espace', then the prefix is returned
%% without an instance name suffix. For example
%% `inst_to_name(espace_sup, espace)' will return `espace_sup'.
%%
%% Instead of generating the full name each time this function is
%% called, we perform the conversion once and cache the result as an
%% entry in the `persistent_term' store. Each entry has the key
%% `{espace, Inst_name, Prefix}', e.g. `{espace, aaa, espace_sup}',
%% and the full name as value, e.g. `espace_sup_aaa'.
%%
%% The use of `persistent_term' store will help speed up all named
%% instances, including the short lived `espace:eval/2' processes.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_to_name(atom(), atom()) -> atom().
inst_to_name(Prefix, Inst_name) ->
    case espace_pterm:get(Inst_name, Prefix) of
        undefined ->
            V = case Inst_name of
                    espace ->
                        Prefix;
                    _ ->
                        list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Inst_name))
                end,
            espace_pterm:put(Inst_name, Prefix, V),
            V;
        V ->
            V
    end.

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

%%--------------------------------------------------------------------
%% @doc wait for etsmgr to (re)start, then ask it to manage our table.
%%
%% There are two occassions where this function is called:
%%
%% <ol>
%%
%% <li> `init' - start/restart of our gen_server that owns a table, in
%% this case we do not have, or know of, the ETS table. So we ask
%% `etsmgr' to create a new table using `etsmgr:new_table/4'. If
%% `etsmgr' is already managing such a table that does not already
%% belong to another process, then that table will be given to
%% us.</li>
%%
%% <li> `recover' - recovery of the `etsmgr' server, in this case we
%% ask `etsmgr' to start managing our ETS table.</li>
%%
%% </ol>
%%
%% @end
%%--------------------------------------------------------------------
-spec wait4etsmgr(atom(), init | recover, atom(), term()) ->
          {ok, pid(), ets:tab()} |
          {error, term()}.
wait4etsmgr(Inst_name, init, Table_name, Table_opts) ->
    etsmgr:wait4etsmgr(Inst_name, 100),
    etsmgr:new_table(Inst_name, Table_name, Table_name, Table_opts);

wait4etsmgr(Inst_name, recover, Table_name, Table_id) ->
    etsmgr:wait4etsmgr(Inst_name, 100),
    etsmgr:add_table(Inst_name, Table_name, Table_id).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc check a term and evaluate it, if it is a function.
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

%%--------------------------------------------------------------------
