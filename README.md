# espace

## Introduction

`espace` is an Erlang implementation of the Tuple Spaces
(or Linda) paradigm. Details can be found on Wikipedia for
[Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language)) and [Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space).

Another good source that describe the paradigm is the following paper:

> Carriero, Nicholas & Gelernter, David. (1989).
> How to Write Parallel Programs: A Guide to the Perplexed.
> ACM Comput. Surv.. 21. 323-357.

The basic idea behind tuple spaces is that an application is given a
storage pool where tuples are added to, the `out` operation, or take from. There are
also many concurrently active worker processes that look for tuples that
match certain patterns, or they block until such a tuple is added to
the pool, probably by another worker via the `out` operation.

## The current implementation

This initial implementation is carried out as part of the [Spawfest
2017](https://spawnfest.github.io/) hackathon. So, due to the short
implementation time (48 hours) it is treated as a proof of concept,
that will be optimized over time.

The basic idea is to make availabe a space for the tuples, currently
`ETS`, along with the four basic operations, `eval`, `out`, `in` and `rd`.

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
