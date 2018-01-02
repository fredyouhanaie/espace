-module(dijkstra).

-export([compute_sp/1, gen_vertex/0, gen_vertex_status/1, gen_nexthop/2, nexthop_worker/0]).

compute_sp({Src, Src}) ->  % shortcut for the degenerate case
    espace_cli:out({shortest_path, {Src, Src}, [Src], 0});

compute_sp({Src, Dst}) ->
    %% generate the vertex_status tuples for the {Src, Dst} path
    espace_cli:eval({dijkstra, gen_vertex_status, [{Src, Dst}]}),
    %% initialize list of vertices to be visited, list is added to by gen_nexthop
    espace_cli:out({visit_list, {Src, Dst}, []}),
    %% start the initial hops from the Src vertex
    espace_cli:eval({dijkstra, gen_nexthop, [{Src, Dst}, Src]}),
    wait_for_completion({Src, Dst}).


% Given a single graph tuple, generate vertex tuples of the form
% {vertex, V, [{V1, C1}, {V2, C2}]}, where {Vn, Cn} are the successor
% vertices with the edge cost. Only one such set is required per graph
gen_vertex() ->
    {[Vlist], _} = espace_cli:rd({graph, '$1'}),
    lists:foreach(fun ({V, SuccList}) ->
			  espace_cli:out({vertex, V, SuccList}) end,
		  Vlist).


% generate vertex_status tuples for all vertices of the graph for a
% specific {Src, Dst} pair
gen_vertex_status({Src,Dst}) ->
    {[Vlist], _} = espace_cli:rd({graph, '$1'}),
    lists:foreach(fun ({V, _SuccList}) ->
			  out_vertex_status({Src, Dst}, V) end,
		  Vlist).


% drop a single initial vertex_status tuple into the TS
out_vertex_status({Src, Dst}, Src) ->
    espace_cli:out({vertex_status, Src, {Src, Dst}, Src, 0});
out_vertex_status({Src, Dst}, V) ->
    espace_cli:out({vertex_status, V, {Src, Dst}, none, infinity}).


% for a given vertex, generate the next hop tuples, one tuple per
% successor vertex, which will be consumed by a nexthop worker
gen_nexthop({Src, Dst}, V) ->
    {[PathCost], _} = espace_cli:rd({vertex_status, V, {Src, Dst}, '_', '$1'}),
    {[SuccList], _} = espace_cli:rd({vertex, V, '$1'}),
    lists:foreach(fun ({NextV, EdgeCost}) ->
			  out_nexthop({Src, Dst}, V, PathCost+EdgeCost, NextV)
		  end,
		  SuccList).


out_nexthop({Src, Dst}, V, HopCost, NextV) ->
    % update the visit_list tuple
    {[Vlist], _} = espace_cli:in({visit_list, {Src, Dst}, '$1'}),
    espace_cli:out({visit_list, {Src, Dst}, [NextV|Vlist]}),
    % drop the tuple in the TS
    espace_cli:out({nexthop, {Src,Dst}, V, NextV, HopCost}).


% wait for a "nexthop" tuple to appear in the TS, then update the next
% vertex, and initiate further hops, if vertex is updated
nexthop_worker() ->
    {[{Src, Dst}, PredV, ThisV, Cost], ThisTuple} = espace_cli:in({nexthop, '$1', '$2', '$3', '$4'}),
    case {Src, Dst} of
	{stop, stop} -> % is this a poison pill?
	    espace_cli:out(ThisTuple); % put it back for the fellow workers
	_ ->
	    {[ThisCost], ThisStatus} = espace_cli:in({vertex_status, ThisV, {Src, Dst}, '_', '$1'}),
	    if
		ThisCost =< Cost ->  % no update needed, return the tuple to the pool unchanged
		    espace_cli:out(ThisStatus);
		ThisCost > Cost ->   % update the vertex status, and initiate further hops
		    espace_cli:out({vertex_status, ThisV, {Src, Dst}, PredV, Cost}),
		    gen_nexthop({Src, Dst}, ThisV)
	    end,
	    % confirm that this hop vertex is done
	    espace_cli:out({visited, {Src, Dst}, ThisV}),
	    nexthop_worker()
    end.


wait_for_completion({Src, Dst}) ->
    % wait for a visited tuple to appear
    {[V], _} = espace_cli:in({visited, {Src, Dst}, '$1'}),
    % update the visit list
    {[Vlist], _} = espace_cli:in({visit_list, {Src, Dst}, '$1'}),
    case lists:delete(V, Vlist) of
	[] -> % no more vertices left to visit
	    trace_path({Src,Dst});
	Rest ->
	    espace_cli:out({visit_list, {Src, Dst}, Rest}),
	    wait_for_completion({Src, Dst})
    end.

% starting with the vertex status of the Dst vertex, generate the
% whole path, going backwards.
trace_path({Src, Dst}) ->
    {[PredV, Cost], _} = espace_cli:in({vertex_status, Dst, {Src, Dst}, '$1', '$2'}),
    trace_path({Src, Dst}, Cost, [PredV,Dst]).

trace_path({Src, Dst}, Cost, Path=[V|_Rest]) ->
    case espace_cli:in({vertex_status, V, {Src, Dst}, '$1', '_'}) of
	{[V], _} -> % we must be at the src vertex, if we are our own parent?
	    drain_TS({vertex_status, '_', {Src, Dst}, '_', '_'}),
	    espace_cli:out({shortest_path, {Src, Dst}, Path, Cost});
	{[PredV], _} ->
	    trace_path({Src, Dst}, Cost, [PredV|Path])
    end.

% remove all tuples matching pattern from the TS
drain_TS(Pattern) ->
    case espace_cli:inp(Pattern) of
	nomatch ->
	    ok;
	_ ->
	    drain_TS(Pattern)
    end.
