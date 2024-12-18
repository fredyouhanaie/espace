# espace

[![Erlang CI](https://github.com/fredyouhanaie/espace/actions/workflows/erlang2.yml/badge.svg)](https://github.com/fredyouhanaie/espace/actions/workflows/erlang.yml) [![Hex.pm](https://img.shields.io/hexpm/v/espace.svg)](https://hex.pm/packages/espace) [![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/espace)

## Introduction

`espace` is an Erlang implementation of the Tuple Spaces
(or Linda) paradigm. Details can be found on Wikipedia for
[Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language))
and [Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space).

Another good source that describes the paradigm well is the following paper:

> Carriero, Nicholas & Gelernter, David. (1989).
> How to Write Parallel Programs: A Guide to the Perplexed.
> ACM Computing Surveys. 21. 323-357.

A copy of the paper can be obtained from the [ACM digital
library](https://dl.acm.org/doi/10.1145/72551.72553).

Further details about the application can be found on the
[wiki pages](https://github.com/fredyouhanaie/espace/wiki).


## Recent changes

* There is an experimental branch where the two `gen_server`s for
  accessing the data in the ETS tables, `espace_tspace` and
  `espace_tspatt`, have eliminated, and instead the client functions
  access the ETS tables directly. See the `exp_serverless` branch for
  details.

## Less recent changes

* Two sets of functions have been moved out of `espace_util` and into
  their own respective modules, `espace_opcount` and `espace_pterm`.

* The module docs are now auto-generated via github actions and
  available online, see <https://fredyouhanaie.github.io/espace>

## Current Status

* The project has been developed and tested on a *Linux* system. Using
  Erlang/OTP 25.3.2 and later, and rebar3.
* The software is under constant development, and SHOULD NOT be
  considered fit for production use.
* Tests are carried out using a set of basic Eunit tests, via `rebar3
  eunit`.
* General documentation can be found on the
  [wiki](https://github.com/fredyouhanaie/espace/wiki) pages.
* Documentation for the source code can be generated via `rebar3
  edoc`.
* The online documentation for the latest commit are also available at
  <https://fredyouhanaie.github.io/espace>.

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

* To generate the documentation that includes all the module functions:
```
rebar3 as dev edoc
```

* To generate the EEP-48 doc chunks:

```
rebar3 as chunks edoc
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
```
* At the erlang shell prompt, if desired, bring up the Observer
```
> observer:start().
```
* Run the tiny test program
```
> cd("Examples/adder1").
> c(adder1).
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
  '$1', '$2'}` and `{sum, '$1', '$2', '$3'}`.

## Using the `observer_cli` plugin

[observer\_cli](https://github.com/zhongwencool/observer_cli) is a
command line based (i.e non-GUI) application that can be used to
visualise various performance metrics of an erlang node. It can be
extended with user supplied plugins, which is what we have here.

The espace plugin, `espace_observer.erl`, will display a single
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

Before running the observer_cli escript ensure the target node is up
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
`eval` op increments the `eval` and `out` counts.

Enjoy!

Fred
