# Example - Matrix Multiplication

The use of
[Matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication)
can be found in many scientific and engineering applications. The
product of two matrices, say `n x m` and `m x p`, is an `n x p` matrix
whose elements can be produced independently of each other. This makes
it a suitable candidate for *results parallelism* based computation,
see the Carriero paper in the top README file.

## `matmul`

This is a simplistic example of an `espace` application for computing
the product of two matrices.

For this example, a matrix is represented as a list of row vectors,
which are in turn represented as lists of integers, for examples the
matrix:

```
1 2 3
4 5 6
7 8 9
```

is represented as:

> [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]

The `espace` application will take a pair of compatible matrices and
produce their product.

The tuples for the input matrices are identified with a type tag,
`matrix`, and an identifier tag, to distinguish between the two,
`mat_a` and `mat_b`, for example:

> {matrix, mat_a, [ [11, 12, 13], [14, 15, 16], [17, 18, 19] ]}

> {matrix, mat_b, [ [21, 22, 23], [24, 25, 26], [27, 28, 29] ]}

A set of one or more workers are used to produce the elements of the
resulting matrix.

To compute element `{R, C}` of the product matrix, the worker needs
the two vectors from row `R` of the first matrix and from column `C`
of the second one, this is called the
[dot product](https://en.wikipedia.org/wiki/Dot_product).

These vectors are generated from the two matrices, and dropped into
the tuple space. These vectors have the following format for the row
vectors of the first matrix, `mat_a`:

```
{row_vector, mat_a, 1, [11, 12, 13]}
{row_vector, mat_a, 2, [14, 15, 16]}
{row_vector, mat_a, 3, [17, 18, 19]}
```

and for the column vectors of the second, `mat_b`:

```
{col_vector, mat_b, 1, [21, 24, 27]}
{col_vector, mat_b, 2, [22, 25, 28]}
{col_vector, mat_b, 3, [23, 26, 29]}
```

Splitting the input matrices into the row vectors is simple, a single
`eval` per matrix will do the job, however, for the second matrix, we
need to take the extra step of splitting the row vectors into the
individual matrix element, and the reassemble as column vectors.

We also generate a set of tuples that act as place holders for the
result matrix. Eac represent an individual dot product computation to
be performed, below is an example of such a tuple:

> {dotprodme, mat_a, 2, mat_b2, 3, mat_c}

When a dot product worker picks up this tuple, it will proceed to `rd`
the corresponding row and column vectors, i.e.

```
{row_vector, mat_a, 2, [14, 15, 16]}
{col_vector, mat_b, 3, [23, 26, 29]}
```

The worker will then produce, `out`, the following tuple:

> {dotprod, mat_c, 2, 3, DotProd}

where `DotProd` is the computed dot product of the two vectors.

A `gatherer` worker process will collect the `dotprod` tuples and once
all of them have been collected, it will create the result matrix.

See the `matmul.erl` module for mor details.

To test the application, there are two sets of data files,
`matmul-1.esp` and `matmul-2.esp`.

To run the application, assuming you have rebar3 available, and
`espace` has already been built via `rebar3 compile`, from the
project's top level directory:

```
$ rebar3 shell

> cd("Examples/matmul").

> c(matmul).

> matmul:start("matmul-1.esp").
[870,906,942]
[1086,1131,1176]
[1302,1356,1410]
ok

> matmul:start("matmul-2.esp").
[1710,1760]
[2254,2320]
[2798,2880]
ok

```

