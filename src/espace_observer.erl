%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% `observer_cli' plugin for `espace'.
%%%
%%% @end
%%% Created : 27 Mar 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_observer).

-export([attributes/1, sheet_header/0, sheet_body/1]).
-export([load_config/0, load_config/1]).

%%-------------------------------------------------------------------

%% the list of operators to show the metrics for
-define(Ops, [in, rd, inp, rdp, out, eval]).

%%-------------------------------------------------------------------

%% The default configuration for the plugin
%%
%% Note that this same configuration is repeated in
%% `config/shell.config' for automatic loading during a `rebar3 shell'
%% session
-define(Default_config,
        [
         {observer_cli,
          [{ plugins,
             [ #{module      => espace_observer,
                 title       => "espace observations",
                 shortcut    => "E",
                 interval    => 2000,
                 sort_column => 1 }
             ]
           }]}
        ]).


%%--------------------------------------------------------------------
%% @doc Load the default `observer_cli' plugin env data.
%%
%% See `load_config/1' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_config() -> ok.
load_config() ->
    load_config(?Default_config).


%%--------------------------------------------------------------------
%% @doc Load the supplied `observer_cli' plugin env data.
%%
%% This is only needed in order to allow a remote `observer_cli' to
%% access the espace plugin.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_config(term()) -> ok.
load_config(Config) ->
    application:load(observer_cli),
    application:set_env(Config).


%%--------------------------------------------------------------------
%% @doc The callback for the attributes section.
%%
%% Presently this will return an empty list.
%%
%% @end
%%--------------------------------------------------------------------
-spec attributes(term()) -> {[any()], term()}.
attributes(Prev_state) ->
    Attrs = [ ],
    Next_state = Prev_state,
    {Attrs, Next_state}.


%%--------------------------------------------------------------------
%% @doc The callback to return the column headings
%%
%% @end
%%--------------------------------------------------------------------
-spec sheet_header() -> [ #{ title => string(),
                             width => pos_integer(),
                             shortcut => string() } ].
sheet_header() ->
    Header = [ {inst_name, 20},
               {tuples, 10},
               {waiting, 10} |
               [{atom_to_list(Op), 10} || Op <- ?Ops ] ],
    [ #{title => T, width => W} || {T, W} <- Header ].


%%--------------------------------------------------------------------
%% @doc The callback to return all the counts for all instances.
%%
%% @end
%%--------------------------------------------------------------------
-spec sheet_body(term()) -> {[list()], term()}.
sheet_body(Prev_state) ->
    Body = [ [Inst | inst_counts(Inst)] || Inst <- inst_names() ],
    Next_state = Prev_state,
    {Body, Next_state}.


%%--------------------------------------------------------------------
%% @doc Return a list of the current instances.
%%
%% The instances are derived from the pterms for `opscounters'.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_names() -> [atom()].
inst_names() ->
    Insts = fun ({{espace, Inst, opcounters}, _}) ->
                    {true, Inst};
                (_) -> false
            end,
    lists:filtermap(Insts, persistent_term:get()).


%%--------------------------------------------------------------------
%% @doc Return the various counts for an instance as a list.
%%
%% The first two list elements are the total tuples and blocked
%% clients for the instance, the rest correspond to the elements in
%% the `Ops' macro.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_counts(atom()) -> [integer()].
inst_counts(Inst_name) ->
    Counts = espace_util:opcount_counts(Inst_name),
    [ inst_tuples(Inst_name), inst_waiting(Inst_name) |
      [ maps:get(Ctr, Counts) || Ctr <- ?Ops ] ].


%%--------------------------------------------------------------------
%% @doc Return the number of tuple in the TS.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_tuples(atom()) -> integer().
inst_tuples(Inst_name) ->
    Tab_id = espace_util:pterm_get(Inst_name, tspace_tabid),
    ets:info(Tab_id, size)-1. %% we discount the entry for the key counter


%%--------------------------------------------------------------------
%% @doc Return the list of blocking clients.
%%
%% This is basically the number of records in the tspatt ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec inst_waiting(atom()) -> integer().
inst_waiting(Inst_name) ->
    Tab_id = espace_util:pterm_get(Inst_name, tspatt_tabid),
    ets:info(Tab_id, size).

%%-------------------------------------------------------------------
