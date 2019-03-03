-module(matmul).

-export([start/1, gatherer/3, matsize_bytag/1, gen_dotprodme/3, dotprod_worker/0]).
-export([gen_col_vectors/1, split_matrix_bytag/1, split_row_vector_bytag/1]).

% Example Matrix Multiplication demo program

% We expect InFile to contain a pair of "out" tuples for each of the
% two matrices.

start(InFile) ->
    % get and save the matrix sizes
    espace:worker({matmul, matsize_bytag, [mat_a]}),
    espace:worker({matmul, matsize_bytag, [mat_b]}),

    % split both matrices into row vectors
    espace:worker({matmul, split_matrix_bytag, [mat_a]}),
    espace:worker({matmul, split_matrix_bytag, [mat_b]}),

    % for the second matrix, split further into elements, then
    % reassemble as column vectors
    espace:worker({matmul, split_row_vector_bytag, [mat_b]}),
    espace:worker({matmul, gen_col_vectors, [mat_b]}),

    % generate the place holders for the result matrix
    % these will be consumed by the dotprod worker processes
    espace:worker({matmul, gen_dotprodme, [mat_a, mat_b, mat_c]}),

    % two worker processes should be sufficient for our demo
    % each worker will take 'dotprodme' tuples and produce 'dotprod' tuples
    espace:worker({matmul, dotprod_worker, []}),
    espace:worker({matmul, dotprod_worker, []}),

    % the final results collector
    espace:worker({matmul, gatherer, [mat_a, mat_b, mat_c]}),

    % Note that none of the above workers will start operating until the
    % two input matrices are dropped into the tuple space.

    % all we need now is the data to get things moving
    espace:infile(InFile),

    % wait for the final matrix, and print it
    {[Matrix_C], _} = espace:rd({matrix, mat_c, '$1'}),
    print_matrix(Matrix_C),

    % Now clean up the Tuple Space
    Tlist = [{matrix, '_', '_'},
	     {matsize, '_', '_', '_'},
	     {row_vector, '_', '_', '_'},
	     {col_vector, '_', '_', '_'},
	     {dotprodme, '_', '_', '_', '_', '_'}],
    drain_tspool(Tlist),
    ok.

drain_tspool([]) ->
    ok;
drain_tspool([Pattern|Rest]) ->
    drain_pattern(Pattern),
    drain_tspool(Rest).

drain_pattern(Pattern) ->
    case espace:inp(Pattern) of
	nomatch ->
	    ok;
	{_,_} ->
	    drain_pattern(Pattern)
    end.

print_matrix(M) ->
    lists:foreach(fun (R) -> io:format("~w~n", [R]) end, M).

gatherer(Tag1, Tag2, Tag3) ->
    % we need to know how many dot product elements to expect and wait for
    {[Nrows], _} = espace:rd({matsize, Tag1, '$1', '_'}),
    {[Ncols], _} = espace:rd({matsize, Tag2, '_', '$2'}),
    ResultElements = Nrows*Ncols,
    % save the count, which will be decremented as the results come in
    espace:out({dotprod_count, ResultElements}),
    % we wait until all done
    wait_for_results(Tag3),
    % put out the poison pill for the dotprod workers
    espace:out({dotprodme, 'STOP', x, x, x, x}),
    % Now let's build and save the result matrix
    espace:out({matrix, Tag3, make_matrix(Tag3, Nrows, Ncols)}),
    ok.

wait_for_results(Tag) ->
    % wait for a new dotprod
    {[Row, Col, Val], _} = espace:in({dotprod, Tag, '$1', '$2', '$3'}),
    % put it back as an 'element', so that we don't count it again
    espace:out({element, Tag, Row, Col, Val}),
    % check the counter
    {[Count], _} = espace:in({dotprod_count, '$1'}),
    case Count of
	1 -> % was this the last one?
	    done; % we have now collected all the dotprod tuples
	_ -> % still need to get some more!
	    espace:out({dotprod_count, Count-1}),
	    wait_for_results(Tag)
    end.

make_matrix(Tag, Nrows, Ncols) ->
    lists:map(fun (RowNum) -> make_row_vector(Tag, RowNum, Ncols) end, lists:seq(1, Nrows)).

make_row_vector(Tag, RowNum, Ncols) ->
    lists:map(fun (ColNum) -> element(Tag, RowNum, ColNum) end, lists:seq(1, Ncols)).

make_col_vector(Tag, ColNum, Nrows) ->
    lists:map(fun (RowNum) -> element(Tag, RowNum, ColNum) end, lists:seq(1, Nrows)).

gen_col_vectors(Tag) ->
    {[Nrows, Ncols], _} = espace:rd({matsize, Tag, '$1', '$2'}),
    lists:foreach(
      fun (ColNum) -> espace:out({col_vector, Tag, ColNum, make_col_vector(Tag, ColNum, Nrows)}) end,
      lists:seq(1, Ncols)
     ).

element(Tag, RowNum, ColNum) ->
    {[Value], _} = espace:in({element, Tag, RowNum, ColNum, '$1'}),
    Value.

gen_dotprodme(Tag1, Tag2, Tag3) ->
    {[Rows1, _], _} = espace:rd({matsize, Tag1, '$1', '$2'}),
    {[_, Cols2], _} = espace:rd({matsize, Tag2, '$1', '$2'}),
    % we generate the tuples sequentially, but we can also parallelize this
    DotProd = fun ({R, C}) -> espace:out({dotprodme, Tag1, R, Tag2, C, Tag3}) end,
    lists:foreach(DotProd, [ {R,C} || R <- lists:seq(1,Rows1), C <- lists:seq(1,Cols2)]),
    ok.

matsize_bytag(Tag) ->
    {[Mat], _} = espace:rd({matrix, Tag, '$1'}),
    Rows = length(Mat),
    Cols = length(hd(Mat)),
    espace:out({matsize, Tag, Rows, Cols}).

split_matrix_bytag(Tag) ->
    {[Matrix], _} = espace:rd({matrix, Tag, '$1'}),
    split_matrix(Matrix, Tag, 1).

split_matrix([], _Tag, _RowNum) ->
    ok;
split_matrix([H|T], Tag, RowNum) ->
    espace:out({row_vector, Tag, RowNum, H}),
    split_matrix(T, Tag, RowNum+1).

split_row_vector_bytag(Tag) ->
    {[Nrows], _} = espace:rd({matsize, Tag, '$1', '_'}),
    lists:foreach(
      fun (RowNum) ->
	      {[RowVec],_} = espace:in({row_vector, Tag, RowNum, '$1'}),
	      split_row_vector(RowVec, Tag, RowNum, 1)
      end,
      lists:seq(1, Nrows)).

split_row_vector([], _Tag, _RowNm, _ColNum) ->
    ok;
split_row_vector([H|T], Tag, RowNum, ColNum) ->
    espace:out({element, Tag, RowNum, ColNum, H}),
    split_row_vector(T, Tag, RowNum, ColNum+1).

dotprod_worker() ->
    {[Tag1, RowNum, Tag2, ColNum, Tag3], Tuple} = espace:in({dotprodme, '$1', '$2', '$3', '$4', '$5'}),
    case Tag1 of
	'STOP' -> % "poison pill"?
	    espace:out(Tuple), % put it back for the fellow workers!
	    done;
	_ ->
	    {[Vec1], _} = espace:rd({row_vector, Tag1, RowNum, '$1'}),
	    {[Vec2], _} = espace:rd({col_vector, Tag2, ColNum, '$1'}),
	    DotProd = dot_prod(Vec1, Vec2),
	    espace:out({dotprod, Tag3, RowNum, ColNum, DotProd}),
	    dotprod_worker()
    end.

dot_prod(Vec1, Vec2) when length(Vec1) == length(Vec2) ->
    lists:sum( lists:zipwith(fun (X,Y) -> X*Y end, Vec1, Vec2) ).
