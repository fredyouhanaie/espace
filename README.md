# espace

## Introduction

`espace` is an Erlang implementation of the Tuple Spaces
(or Linda) paradigm. Details can be found on Wikipedia for
[Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language)) and [Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space).

Another good source that describes the paradigm well is the following paper:

> Carriero, Nicholas & Gelernter, David. (1989).
> How to Write Parallel Programs: A Guide to the Perplexed.
> ACM Computing Surveys. 21. 323-357.

The basic idea behind tuple spaces is that an application is given
a storage pool where tuples are added to via the `out` operation, or
taken from the pool via the `in` or `rd` operations.

There are also many concurrently active worker processes that look for
tuples that match certain patterns, or they block until such a tuple is
added to the pool, probably by another worker via the `out` operation.

The worker processes are started via the `eval` operation. A worker can
in turn start further workers via the `eval` operation.

A typical application will start with a series of `out` and `eval`
oprations. Some of the worker processes will then, using the `in` or `rd`
operations, pick the tuples added via the `out` operations, process them,
then `out` the results. Other worker processes can then pick the results
and process them further.

## The current implementation

This initial implementation is carried out as part of the [Spawfest
2017](https://spawnfest.github.io/) hackathon. So, due to the short
implementation time (48 hours) it is treated as a proof of concept,
that will be optimized over time.

To start with, we will provide a space for the tuples, currently `ETS`,
along with the four basic operations, `eval`, `out`, `in` and `rd`.

## Current Status

* The project has been developed and tested on a *Linux* system.
* Having issues with getting `rebar3` to compile the code - newbie issue!
* The modules are, for now, being compiled manually via `make`.
* All seems to be working, with manual test.
* No test scripts, yet.
* Not an scalable implementation, yet.

## To check out the application

For now, you need to build and run it manually :-(

* Change to the `src/` directory, `cd src`, then run `make`
* To run the application
  * start the shell with SASL
    > `erl -boot start_sasl`
  * start the `espace` application
    > `application:start(espace).`
  * start the test adder
    > `espace_test1:start().
  * use the table viewer in `observer:start` to see the progress of the adder.
  * try adding new tuples to the pool, e.g.
    > `espace_cli:out({add, 42, 43}).`
  * You should see a `{sum, 42, 43, 85}` in the `tspool` table
  * There will always be a `{add, '$1', '$2'}` pattern waiting in the `tspool_patt` table.


## Organization

`espace` has been built as an application, `espace_app`. Presently only
one instance of the application can run on a given node.

Once the server application is started, the client applications can use
the `espace_cli` module to perform any of the four operations. In fact
one can kick off the whole process by calling a purpose written bootstrap
function, which can in turn perform `out` and `eval` operations, e.g.

> `espace_cli:eval({Mod, Func, Args})`.

On the server side there are two main components:

- `tspool_srv` is a `gen_server` that manages the TS storage pool. This server will handle the `out`, `in` and `rd` requests.
- `wkpool_sup` is a `supervisor` that manages worker processes, this in turn has the following components:
  - `wkpool_srv` is a `gen_server` that will receive the `eval` requests, and pass the request to the worker supervisor.
  - `worker_sup` is a `supervisor` that will create a new child process with the supplied `{M,F,A}` function.

### TSPOOL - Tuple Space Pool

The tuples are kept in an ETS table with two columns, A unique ref,
and the tuple itself. A single `gen_server` handles all access to the
ETS table. This is not scalable and will be addressed in due course.

For the cases where no matching tuple is found, a second ETS table is
used to remember the pattern, the client PID and a unique reference
for this specific request. The unique reference will be used by the
`tspool_srv` client to block on a receive for the unique reference.

Each `out` operation, after inserting the new tuple, will scan through
the waiting list and if it matches, the client is sent a message with
the unique reference and the entry is removed from the waiting patterns
table. Once the waiting client receives the unique reference, it will
attempt the `rd` or `in` operation again. Note that the client is not
handed the tuple, but notified to ask for it again. If more than one
client have waiting patterns that match the tuple, then they will end
up competing for it.

### WKPOOL - Worker Pool

Each `eval` operation, is executed within a child process of a
`simple_one_for_one` supervisor. No attempt is made to throttle the
number of `eval` processes.
