%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2022, Fred Youhanaie
%%% @doc
%%%
%%% Provide access to the operation counters.
%%%
%%% @end
%%% Created : 26 Sep 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(espace_opcount).

%%--------------------------------------------------------------------

%% API
-export([new/0, incr/1, counts/0, reset/0]).
-export([new/1, incr/2, counts/1, reset/1]).

%%--------------------------------------------------------------------

-type(espace_op() :: in | rd | inp | rdp | out | eval).

%%--------------------------------------------------------------------

-record(opctr, {in, rd, inp, rdp, out, eval}).

%% the counter elements by name
-define(Opctr_names, record_info(fields, opctr)).
-define(Opctr_size,  record_info(size, opctr)).

-define(Opctr_ref, espace_pterm:get(Inst_name, opcounters)).

%%--------------------------------------------------------------------
%% @doc Create a new ops counter array for the unnamed instance.
%%
%% See new/1 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> ok.
new() ->
    new(espace).

%%--------------------------------------------------------------------
%% @doc Create a new ops counter array for a named instance.
%%
%% The array will have one counter per espace operation. The counters
%% ref is saved in as a persistent term.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(atom()) -> ok.
new(Inst_name) ->
    Ctr_ref = counters:new(?Opctr_size, []),
    espace_pterm:put(Inst_name, opcounters, Ctr_ref).

%%--------------------------------------------------------------------
%% @doc Increment a single espace op counter for the unnamed instance.
%%
%% See incr/1 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec incr(espace_op()) -> ok.
incr(Op) ->
    incr(espace, Op).

%%--------------------------------------------------------------------
%% @doc Increment a single espace op counter for a named instance.
%%
%% In the interest of keeping the code simple, the counter index of
%% each op corresponds to the position of `Op' in the record tuple,
%% which ranges from 2 to 7.
%%
%% @end
%%--------------------------------------------------------------------
-spec incr(Inst_name :: atom(), Op :: espace_op()) -> ok.
incr(Inst_name, in)   -> counters:add(?Opctr_ref, #opctr.in,   1);
incr(Inst_name, rd)   -> counters:add(?Opctr_ref, #opctr.rd,   1);
incr(Inst_name, inp)  -> counters:add(?Opctr_ref, #opctr.inp,  1);
incr(Inst_name, rdp)  -> counters:add(?Opctr_ref, #opctr.rdp,  1);
incr(Inst_name, out)  -> counters:add(?Opctr_ref, #opctr.out,  1);
incr(Inst_name, eval) -> counters:add(?Opctr_ref, #opctr.eval, 1).

%%--------------------------------------------------------------------
%% @doc Return the current counts for the unnamed instance as a map.
%%
%% See counts/1 for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec counts() -> #{espace_op() => integer()}.
counts() ->
    counts(espace).

%%--------------------------------------------------------------------
%% @doc Return the current counts for a named instance as a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec counts(atom()) -> #{espace_op() => integer()}.
counts(Inst_name) ->
    C = ?Opctr_ref,
    Counts = [ counters:get(C, Idx) || Idx <- lists:seq(2, ?Opctr_size) ],
    maps:from_list(lists:zip(?Opctr_names, Counts)).

%%--------------------------------------------------------------------
%% @doc Reset all the op counters of the unnamed instance.
%%
%% See `reset/1' for details.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset() -> ok.
reset() ->
    reset(espace).

%%--------------------------------------------------------------------
%% @doc Reset all the op counters of a named instance.
%%
%% This function has been provided for investigating an application.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(atom()) -> ok.
reset(Inst_name) ->
    C = ?Opctr_ref,
    [ counters:put(C, Idx, 0) || Idx <- lists:seq(2, ?Opctr_size) ],
    ok.

%%--------------------------------------------------------------------
