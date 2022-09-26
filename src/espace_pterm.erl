%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2022, Fred Youhanaie
%%% @doc
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
%%% Created : 26 Sep 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_pterm).

%% API
-export([erase/1, get/2, put/3]).

%%--------------------------------------------------------------------

-type(pt_key() :: {espace, atom(), atom()}).

%%--------------------------------------------------------------------

%% used for locating all keys
-define(PTerm_key(Inst_name), {espace, Inst_name, _}).

%% used for locating specific keys
-define(PTerm_key(Inst_name, Key), {espace, Inst_name, Key}).

%%--------------------------------------------------------------------
%% @doc Lookup the instance `Key' for `Inst_name'.
%%
%% If the persistence term does not exist, `undefined' is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get(atom(), atom()) -> undefined | term().
get(Inst_name, Key) ->
    PT_key = ?PTerm_key(Inst_name, Key),
    persistent_term:get(PT_key, undefined).

%%--------------------------------------------------------------------
%% @doc Create an `espace' persistent term.
%%
%% @end
%%--------------------------------------------------------------------
-spec put(atom(), atom(), term()) -> ok.
put(Inst_name, Key, Term) ->
    PT_key = ?PTerm_key(Inst_name, Key),
    persistent_term:put(PT_key, Term).

%%--------------------------------------------------------------------
%% @doc Remove all the persistent terms for a given `espace' instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec erase(atom()) -> [] | [{pt_key(), term()}].
erase(Inst_name) ->
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
