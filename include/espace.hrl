%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% The data definitions for the `espace' application.
%%%
%%% @end
%%% Created : 03 Jun 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%% convenience functions for logging
-define(Log_base, #{fun_name=>?FUNCTION_NAME}).
-define(Log_info(Log_map), ?LOG_INFO(maps:merge(?Log_base, Log_map))).
-define(Log_warning(Log_map), ?LOG_WARNING(maps:merge(?Log_base, Log_map))).

%%--------------------------------------------------------------------
%% table definitions
%%
%% We have a map for each ETS tables. The set of parameters for each
%% map are used by the clients and the functions dealing with the
%% `etsmgr' server.
%%
%% `name' is the table name used with `etsmgr'.
%%
%% `opts' are the ETS table options.
%%
%% `idkey' is the key used within the `persistent_term' entry for the
%% ETS table id.
%%
-define(Tables, #{tspace => #{name => espace_tspace, 
                              opts => [ordered_set, public,
                                       {write_concurrency, true},
                                       {read_concurrency, true}],
                              idkey => tspace_tabid},

                  tspatt => #{name => espace_tspatt,
                              opts => [ordered_set, public,
                                       {write_concurrency, true},
                                       {read_concurrency, true}],
                              idkey => tspatt_tabid}}).

%% A convenience macro for accessing the table parameter, for example
%% `?Table_param(tspace, idkey)' will return the `idkey' part of the
%% persistent_term record for the `tspace' table.
%%
-define(Table_param(Tab, Par),
        maps:get(Par, maps:get(Tab, ?Tables))).

%%--------------------------------------------------------------------
