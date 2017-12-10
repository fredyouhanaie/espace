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

## The current implementation

This initial implementation is carried out as part of the [Spawfest
2017](https://spawnfest.github.io/) hackathon. So, due to the short
implementation time (48 hours) it is treated as a proof of concept,
that will be optimized over time.

The basic idea is to make availabe a space for the tuples, currently
`ETS`, along with the four basic operations, `eval`, `out`, `in` and `rd`.

## Current Status

* The project has been developed on a *Linux* system.
* Having issues with getting `rebar3` to compile the code - newbie issue!
* The modules are being compiled manually via `make`.
* The worker side looks good.
* On the TS side, `out` is good, but the pattern matching side, `in` and `rd`, is still work in progress.
* No test scripts, yet.

## To check out the application

For now, you need to build and run it manually :-(

* Change to the `src/` directory, `cd src`, then run `make`
* To run the application
  * start the shell with SASL
    > `erl -boot start_sasl`
  * start the `espace` application
    > `application:start(espace).`

## Organization

`espace` has been build as an application, `espace_app`. Presently only
one instance of the application can run on a given node.

Once the server application is started, the client applications can use
the `espace_cli` module to perform any of the four operations. In face
one can kick off the whole process calling single purpose written boot
function, e.g.

> `espace_cli:eval({Mod, Func, Args})`.

On the server side there are two main components:

- `tspool_srv` is a `gen_server` that manages the TS storage pool. This server will handle the `out`, `in` and `rd` requests.
- `wkpool_sup` is a `supervisor` that manages worker processes, this in turn has the following components:
  - `wkpool_srv` is a `gen_server` that will receive the `eval` requests, and pass the request to the worker supervisor.
  - `worker_sup` is a `supervisor` that will create a new child process that in turn will `apply` the supplied `{M,F,A}` function.
