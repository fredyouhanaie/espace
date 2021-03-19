% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% A set of utility functions to support the rest of the espace
%%% modules.
%%%
%%% When `espace' instances are started, a number of instance specific
%%% `persistent_term' entries are created. The `pterm_*' functions are
%%% used for handling these terms.
%%%
%%% The `persistent_term' entries are expected to have the following
%%% format: `{espace, Inst_name, Key}', where, `Key' identifies the
%%% particular `espace' item, such as server name to ETS table name,
%%% and `Inst_name' is the espace instance name, which is the same as
%%% the application name. For the unnamed instance the `Inst_name' is
%%% `espace'.
%%%
%%% @end
%%% Created : 11 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_util).

%% API
-export([eval_out/1, eval_out/2]).
-export([inst_to_name/2, wait4etsmgr/4]).
-export([pterm_erase/1, pterm_get/2, pterm_put/3]).

%%--------------------------------------------------------------------

-type(pt_key() :: {espace, atom(), atom()}).

%% used for locating all keys
-define(PTerm_key(Inst_name), {espace, Inst_name, _}).

%% used for locating specific keys
-define(PTerm_key(Inst_name, Key), {espace, Inst_name, Key}).

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
%% instances, including the short lived `eval/2' processes.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_to_name(atom(), atom()) -> atom().
inst_to_name(Prefix, Inst_name) ->
    case pterm_get(Inst_name, Prefix) of
        undefined ->
            V = case Inst_name of
                    espace ->
                        Prefix;
                    _ ->
                        list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(Inst_name))
                end,
            pterm_put(Inst_name, Prefix, V),
            V;
        V ->
            V
    end.


%%--------------------------------------------------------------------
%% @doc Lookup the instance `Key' for `Inst_name'.
%%
%% If the persistence term does not exist, `undefined' is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec pterm_get(atom(), atom()) -> undefined | term().
pterm_get(Inst_name, Key) ->
    PT_key = ?PTerm_key(Inst_name, Key),
    persistent_term:get(PT_key, undefined).


%%--------------------------------------------------------------------
%% @doc Create an `espace' persistent term.
%%
%% @end
%%--------------------------------------------------------------------
-spec pterm_put(atom(), atom(), term()) -> ok.
pterm_put(Inst_name, Key, Term) ->
    PT_key = ?PTerm_key(Inst_name, Key),
    persistent_term:put(PT_key, Term).


%%--------------------------------------------------------------------
%% @doc Remove all the persistent terms for a given `espace' instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec pterm_erase(atom()) -> [] | [{pt_key(), term()}].
pterm_erase(Inst_name) ->
    Erase_term = fun ({K = ?PTerm_key(Inst), V})
                       when Inst == Inst_name ->
                         case persistent_term:erase(K) of
                             true ->
                                 {true, {K, V}};
                             false ->
                                 false
                         end;
                     (_) ->
                         false
                 end,

    Pterms = persistent_term:get(),
    lists:filtermap(Erase_term, Pterms).


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
-spec wait4etsmgr(atom(), init | recover, atom(), term()) -> {ok, pid(), ets:tab()} | {error, term()}.
wait4etsmgr(Inst_name, init, Table_name, Table_opts) ->
    etsmgr:wait4etsmgr(Inst_name),
    etsmgr:new_table(Inst_name, Table_name, Table_name, Table_opts);

wait4etsmgr(Inst_name, recover, Table_name, Table_id) ->
    etsmgr:wait4etsmgr(Inst_name),
    etsmgr:add_table(Inst_name, Table_name, Table_id).


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
