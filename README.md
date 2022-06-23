# espace

[![Erlang CI](https://github.com/fredyouhanaie/espace/actions/workflows/erlang.yml/badge.svg)](https://github.com/fredyouhanaie/espace/actions/workflows/erlang.yml)

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

* We have a plugin for `observer_cli`, see the module
  `src/espace_observer.erl`. You can find details further below

* The `rebar3_bench` scripts have been removed, and will be made
  available as a separate project.

* When an `espace` server terminates, all the waiting clients, i.e
  those that have called `in/1,2` or `rd/1,2`, will be returned the
  `quit` atom, instead of blocking indefinitely. This may break
  applications that are expecting a return of type `{list(),
  tuple()}`, or those that assume a return implies that the tuple
  is/was present in the TS.

* Some performance improvements when mapping instance names to actual
  server objects (`espace_util:inst_to_name/2`).

* A set of op counters is maintained for each active instance. The
  counters are autoincremented for each espace operation. The counts
  can be accessed via the `espace_util:opcount_counts/0,1`
  functions. You can find details in the module docs.

* For each instance there is a set of `persistent_term` records, that
  can be used for various diagnostic tools. You can find details in
  the `espace_util` module docs, look for the `pterm_*` functions.


## Current Status

* The project has been developed and tested on a *Linux* system. Using
  Erlang/OTP 21.3 and later, and rebar3.
* The software is under constant development, and SHOULD NOT be
  considered fit for production use.
* Tests are carried out using a set of basic Eunit tests, via `rebar3
  eunit`.
* General documentation can be found on the [wiki
  pages](https://github.com/fredyouhanaie/espace/wiki).
* Documentation for the source code can be generated via `rebar3
  edoc`.


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
* Ensure that you have the erlang binaries and rebar3 in your shell
  path
* Build the application
```
$ rebar3 do clean,compile
```
* Start the application via the shell
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
* There will always be two patterns in the `espace_tspatt` table, `{add,
  '$1', '$2'}` and `{sum, '$1', '$2', '$3'}`. You can check that the
  waiting pattern pids match the child process pids of `worker_sup` on
  the Applcation tab.


## Using the `observer_cli` plugin

[observer\_cli](https://github.com/zhongwencool/observer_cli) is a
command line based (i.e non-GUI) application that can be used to
visualise various performance metrics of an erlang node. It can be
extended with user supplied plugins, which is what we have here.

The espace plugin, `espace_observer.erl`, will list display a single
row for each active instance of espace. Each row will display the
instance name, the number of tuples in the TS, the number of waiting
(blocked) clients, and the counters for the six operations.

The CUI/TUI can be started with the shell as shown below:

```
$ rebar3 shell
> espace:start().
> %% start your espace application
> observer_cli:start().
```

In the observer_cli screen press `P<return>` to display the espace screen.

You can also run `observer_cli` as a standalone command, see the notes
in the [escriptize
section](https://github.com/zhongwencool/observer_cli#escriptize).

Before running the observer_cli escript, start the target node is up
and running, e.g.

```
$ rebar3 shell --name espace@localhost
```

In a separate terminal window:

```
observer_cli espace@localhost
```

Back in the espace shell, try starting a couple of instances:

```
> espace:start().
> espace:start(aaa).
> espace:out({five, 2+3}).
> espace:eval(aaa, {five, fun () -> 2+3 end})
```

The above sequence will result in two rows in the observer_cli plugin
screen, `espace` and `aaa`. Each will show 1 tuple and one `out`. The
`aaa` instance will also show a count of 1 for `eval`. Note that each
`eval` increments the `eval` and `out` counts.

Enjoy!

Fred
