%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc Low level access functions for the tuple space.
%%%
%%% These are the functions that directly access the espace
%%% tables. They replace the original `gen_server' based access
%%% methods.
%%%
%%% The functions in this module are expected to be called from within
%%% other `espace' modules.
%%%
%%% @end
%%% Created : 29 May 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_cli).

-export([add_tuple/2, get_tuple/3]).

%%--------------------------------------------------------------------

-include_lib("include/espace.hrl").

%%--------------------------------------------------------------------
%% @doc perform a data input operation.
%%
%% Performs one of the `in', `rd', `inp' or `rdp' operations.
%%
%% Multiple processes may access the ETS table concurrently, and even
%% "compete" for the same tuple. We will ensure that in the case of
%% `in' and `inp' only one of them will succeed in taking the tuple.
%%
%% If a match is found we retrun `{match, {list(), tuple()}}', where
%% the `list()' will contain the `$N' fields in `Pattern', and the
%% `tuple()' is the entire matched tuple record. If match is not
%% found, we return `{nomatch}' for `inp' and `rdp', and `{nomatch,
%% reference()}' for `in' and `rd'. For the latter the calling process
%% should wait/block for two types of messages `{reference(), retry,}'
%% and `{reference(), quit}'. These messages are handled in the
%% `espace' module.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_tuple(atom(), in|rd|inp|rdp, tuple()) ->
          {nomatch} | 
          {nomatch, reference()} |
          {match, {list(), tuple()}}.
get_tuple(Inst_name, Espace_op, Pattern) ->
    Tab_id = espace_pterm:get(Inst_name, ?Table_param(tspace, idkey)),
    Match = ets:match(Tab_id, {'$0', Pattern}, 1),
    check_match(Inst_name, Espace_op, Pattern, Tab_id, Match).

%%--------------------------------------------------------------------
%% @doc add a `Tuple' to the tuple space.
%%
%% This function implements the `out' operation. It is also called at
%% the end of an `eval' operation.
%%
%% Once the `Tuple' is added to the tuple space, the patterns table is
%% searched for any clients waiting for such a tuple. The search is
%% performed in a separate concurrent process, so that the calling
%% client process can continue with its own tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_tuple(atom(), tuple()) -> done.
add_tuple(Inst_name, Tuple) ->
    Tab_id = espace_pterm:get(Inst_name, ?Table_param(tspace, idkey)),
    Tab_key = erlang:unique_integer([monotonic, positive]),
    ets:insert(Tab_id, {Tab_key, Tuple}),
    erlang:spawn(fun () -> check_waitlist(Inst_name, Tuple) end),
    done.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc add a pattern for a waiting client.
%%
%% For each blocking client, we keep a unique reference, the pattern
%% being blocked on, and the client pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_pattern(atom(), reference(), tuple(), pid()) -> true.
add_pattern(Inst_name, Cli_ref, Pattern, Cli_pid) ->
    Tab_id = espace_pterm:get(Inst_name, ?Table_param(tspatt, idkey)),
    Key = erlang:unique_integer([monotonic,positive]),
    ets:insert(Tab_id, {Key, Cli_ref, Pattern, Cli_pid}).

%%--------------------------------------------------------------------
%% @private
%% @doc Start the scan of the table of clients waiting for `Tuple'.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_waitlist(atom(), tuple()) -> done.
check_waitlist(Inst_name, Tuple) ->
    Tab_id = espace_pterm:get(Inst_name, ?Table_param(tspatt, idkey)),
    check_tuple(Tuple, Tab_id, ets:first(Tab_id)).

%%--------------------------------------------------------------------
%% @private
%% @doc Check `Tuple' against the patterns currently being waited for
%% by `in' or `rd' clients.
%%
%% The whole patterns (`tspatt') table is scanned sequentially. As
%% matching patterns are found, the corresponding client is notified
%% to retry the `in' or `rd' operation.
%%
%% It is possible that more than one process may be scanning for the
%% same target pattern. However, only one of them will notify the
%% client, which will be the first process to successfully `take' the
%% ETS record from the patterns table.
%%
%% Each record in the table is accessed 2-3 times:
%%
%% <ol>
%% <li>During the `first/next' scan.</li>
%% <li>The lookup just before the test for match</li>
%% <li>The removal of the tuple when there is a match</li>
%% </ol>
%%
%% The table may be scanned concurrently by multiple independent
%% processes. While each of the above operations are atomic, we need
%% the whole sequence to "appear" atomic to the client. The tuple will
%% only be removed and the client notified if the removal is
%% successful. That is, the first scanner that manages to remove the
%% tuple
%%
%% @end
%%--------------------------------------------------------------------
-spec check_tuple(tuple(), ets:tid(), '$end_of_table'|integer()) -> done.
check_tuple(_Tuple, _Tab_id, '$end_of_table') ->
    done;
check_tuple(Tuple, Tab_id, Key) ->
    case ets:lookup(Tab_id, Key) of
        [] -> %% another scanner has already taken the tuple
            %% and notified the client
            nomatch;  %% continue with the scan
        [{Key, Cli_ref, Pattern, Cli_pid}] ->
            case ets:test_ms(Tuple, [{Pattern,[],['$$']}]) of
                {ok, false} ->
                    nomatch;  %% continue with the scan
                _ ->
                    case ets:take(Tab_id, Key) of
                        [] -> %% another scanner has already taken the tuple
                            %% and notified the client
                            nomatch; %% continue with the scan
                        _ ->
                            Cli_pid ! {Cli_ref, retry}
                    end
            end
    end,
    check_tuple(Tuple, Tab_id, ets:next(Tab_id, Key)).

%%--------------------------------------------------------------------

-define(OP_BLOCKS(Op),  Op == in orelse Op == rd).
-define(OP_REMOVES(Op), Op == in orelse Op == inp).

%%--------------------------------------------------------------------
%% @private
%% @doc Search for a pattern and return the matching tuple.
%%
%% We first look for a match and if a match is found, we get the
%% entire record. Since multiple processes may be actively looking for
%% the same tuple, we make sure that the same tuple cannot be taken
%% (`in' or `inp') by more than one process.
%%
%% If a matching tuple is not found, we either block or return
%% `nomatch', depending on the type of the operation.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_match(atom(), in|rd|inp|rdp, tuple(), ets:tid(), '$end_of_table'|term()) ->
          {nomatch} | 
          {nomatch, reference()} |
          {match, {list(), tuple()}}.
check_match(Inst_name, Espace_op, Pattern, _Tab_id, '$end_of_table') ->
    %% no match was found
    case ?OP_BLOCKS(Espace_op) of
        true -> %% only "in" and "rd" should block on no match
            Cli_pid = self(),
            Cli_ref = make_ref(), %% the client should wait for this ref
            add_pattern(Inst_name, Cli_ref, Pattern, Cli_pid),
            {nomatch, Cli_ref};
        false ->
            {nomatch}
    end;

check_match(Inst_name, Espace_op, Pattern, Tab_id, {[[Tab_key|Fields]], Continuation}) ->
    %% we have a match
    T = case ?OP_REMOVES(Espace_op) of
            true ->
                ets:take(Tab_id, Tab_key);
            false ->
                ets:lookup(Tab_id, Tab_key)
        end,
    TT = case T of
             [] -> %% tuple already taken, try the next match
                 Match = ets:match(Continuation),
                 check_match(Inst_name, Espace_op, Pattern, Tab_id, Match);
             [{Tab_key, Tuple}] ->
                 Tuple
         end,
    {match, {Fields, TT}}.

%%--------------------------------------------------------------------
