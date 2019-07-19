# espace

[![Build Status](https://travis-ci.com/fredyouhanaie/espace.svg?branch=master)](https://travis-ci.com/fredyouhanaie/espace)

## Introduction

`espace` is an Erlang implementation of the Tuple Spaces
(or Linda) paradigm. Details can be found on Wikipedia for
[Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language)) and [Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space).

Another good source that describes the paradigm well is the following paper:

> Carriero, Nicholas & Gelernter, David. (1989).
> How to Write Parallel Programs: A Guide to the Perplexed.
> ACM Computing Surveys. 21. 323-357.

Further details can be found on the [wiki pages](https://github.com/fredyouhanaie/espace/wiki).

## Recent changes

* The [`etsmgr`](https://github.com/fredyouhanaie/etsmgr) application
  is used to ensure the tuple space and pattern data held in ETS
  tables survive the server restarts.
* As we have started using the `handle_continue` callback of
  gen_server, we can only support OTP versions from 21.0 onwards.

## Less resent changes

* `eval` now behaves like the original Linda specification, i.e. like
  `out`, it takes a tuple and adds it to the tuple space, however, if
  the tuple contains any expressions, for espace they are `fun`
  expressions, those expressions are replaced with their respective
  values before adding the tuple to the tuple space. See the module
  documentation for `espace:eval/1,2` for details.
* A new module has been created `espace_util`, which contains the
  current, and future, support functions.

* The `worker` function now returns the pid of the child process.
* The `out` operation is now synchronous, `out` will not return until
  after the tuple has been inserted in the tuple space.
* A separate
  [examples](https://github.com/fredyouhanaie/espace-examples)
  repository has been created to host larger examples,
* Instead of single `espace` application on a node, you can now create
  multiple, independent, named instances on the same node.

## Upcoming changes

* Some work is planned for performance measurement and tracing.

## Current Status

* The project has been developed and tested on a *Linux* system. Using
  Erlang/OTP 20 and 21, and rebar3.
* Tests are carried out using a set of basic Eunit tests, via `rebar3 eunit`.
* General documentation can be found on the [wiki pages](https://github.com/fredyouhanaie/espace/wiki).
* Documentation for the source code can be generated via `rebar3 docs`.
* There are some example programs in the `Examples/` directory.

## Build and testing

`rebar3` is used throughout for all stages of the build and test. All
the below commands should be run from the top level directory:

* To compile the code:
```
rebar3 do clean,compile
```

* To run the tests:
```
rebar3 eunit
```

* To run dialyzer:
```
rebar3 dialyzer
```

* To generate the documentation:
```
rebar3 edoc
```

## To try out the application

* Change to the top level directory of the project
* ensure that you have the erlang binaries and rebar3 in your shell path
* build the application
```
$ rebar3 do clean,compile
```
* Start the application
```
$ rebar3 shell
> espace:start().
```
* At the erlang shell prompt, if desired, bring up the Observer
```
> observer:start().
```
* Run the tiny test program
```
> cd("Examples/adder1").
> adder1:start().
```
  * This will kick off two worker processes via `eval`.
  * One will, continuously, wait for an `{add, X, Y}` tuple to appear in
    the pool, and it will then add the two numbers and `out` their sum as
    `{sum, X, Y, X+Y}`.
  * The second worker will, continuously, wait for a `{sum,X,Y,Z}` tuple,
    and it will then print the contents via `io:format/2`
  * Following the `eval` calls, three `{add, X, Y}` tuples are added
    to the pool, which result in the first worker to pick them up and
    generate the corresponding `{sum, X, Y, X+Y}` tuples. These are in
    turn picked up by the second worker, which in turn prints the
    result to the terminal.
* You can use the table viewer in the Observer to see the progress of
  the two workers.
* try adding new tuples to the pool, e.g.
```
> espace:out({add, 42, 43}).
```
* There will always be two patterns in the `tspace_patt` table, `{add,
  '$1', '$2'}` and `{sum, '$1', '$2', '$3'}`. You can check that the
  waiting pattern pids match the child process pids of `worker_sup` on
  the Applcation tab.


Enjoy!

Fred
