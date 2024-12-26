
Fred Youhanaie <fyrlang@anydata.co.uk>

Copyright 2017-2024 Fred Youhanaie

# Introduction

`espace` is an Erlang implementation of the Tuple Spaces (or Linda)
paradigm. Details of the paradigm can be found on Wikipedia for
[Linda](https://en.wikipedia.org/wiki/Linda_(coordination_language))
[Tuple Spaces](https://en.wikipedia.org/wiki/Tuple_space).

Another good source that describes the paradigm well is the following
paper, which ACM have kindly made freely available in their [digital
library](https://dl.acm.org/doi/10.1145/72551.72553):

> Carriero, Nicholas & Gelernter, David. (1989).
> How to Write Parallel Programs: A Guide to the Perplexed.
> ACM Computing Surveys. 21. 323-357.

`espace` allows one to implement parallel algorithms where tasks
(worker processes) running concurrently communicate and coordinate
through the tuple space, rather than the sending processes requiring
to be aware of the receiving processes in order to send messages
directly to them.

The basic idea behind tuple spaces is that an application is given a
storage pool where tuples are added to it via the `out` and `eval`
operations, or taken from the pool via the `in/inp` and `rd/rdp`
operations.

These six operations are all that is needed for communication and
coordination among the concurrent processes.

An application that uses this system will have many concurrently
active worker processes that look, and optionally, wait for tuples
that match a desired pattern. The waiting workers will block until a
matching tuple is added to the pool by another worker using the
`out`/`eval` operations.

The worker processes are started via the `espace:worker/1,2`
function. A worker can in turn start further workers by calling
`worker/1,2`.

A typical application will start with a series of `out` and `eval`
operations. Some of the worker processes will then, using the `in` or
`rd` operations, pick the tuples added via the `out` operations,
process them, then `out` the results. Other worker processes can then
pick the results and process them further.

The implementation provides a space for the tuples, currently `ETS`,
along with the six basic operations, `eval`, `out`, `in`, `inp`, `rd`
and `rdp`.

# _eval_ vs _worker_

The `eval` operation behaves in the same way as `out`, with the
exception that if there are any `fun` expressions in the supplied
tuple, they will be replaced with their respective function values
before being added to the tuple space. The evaluation of the `fun`
expression will be carried out in a separate process. Which makes
`eval` the main method of starting concurrent processes.

Two forms of function elements are recognized: A zero arity `fun`
expression, `eval({..., fun () -> expr end, ...})` or `eval({..., fun
Fun_name/0, ...})`, and a tuple with two elements, an `N` arity `fun`
expression together with a list of length `N`, `eval({..., {fun (A, B,
...) -> expr end, [expr, expr, ...]}, ...})` or `eval({..., {fun
Fun_name/N, [expr, expr, ...]}, ...})`.  In the second form, the arity
can be zero, however, an empty arg list, `[]`, must accompany the
function.

For example, `eval({sum, 2, 5, fun () 2+5 end})` will
result in the tuple `{sum, 2, 5, 7}`. This has the same
effect as `eval({sum, 2, 5, 2+5})`, however, the main
difference is that in the former a new process will be created to
evaluate the sum. The advantage of using `eval` is where the
function(s) perform complex tasks, and we prefer to let the main
client to continue its work while the computation is being carried
out in the background.

The `worker/1,2` function is a convenient function to spawn new
processes in the background. The return value of the function is
discarded. Typically this is useful for long running processes that
repeatedly wait for a tuple, process it and produce some output.

The `worker/1,2` function takes a tuple that is of the form `{Mod,
Fun, [Args]}` or `fun` expression like those accepted by `eval/1,2`.

# Organization of the Project Modules

`espace` has been built as an OTP application, `espace_app`. Multiple
instances of the application can run concurrently on the same node
without interfering with each other.

The application server should be started with `espace:start/1,2`.
Once the server is running, the client processes can use the `espace`
module to perform any of the six operations. In fact, one can kick off
the whole process by calling a purpose written bootstrap function,
which can in turn perform `out` and `eval` operations.

The top level supervisor started by `espace_app` is `espace_sup`. This
supervisor manages the three `gen_server`s, as described below:

* `etsmgr_srv` is a `gen_server` that adds resiliency to the ETS
  tables, should any of their owner servers restart due to some
  fault. A single instance of `etsmgr_srv` is used for managing the
  two ETS tables. You can find further details in the [project
  repo](https://github.com/fredyouhanaie/etsmgr).

* `espace_tspace_srv` maintains the `espace_tspace` ETS table, the
  main tuple space pool. If a requested pattern does not exist and the
  request is a blocking one, i.e. `in` or `rd`, then the pattern is
  forwarded to `espace_tspatt_srv`. In case of an `out` request, the
  tuple is stored in the table, and `espace_tspatt_srv` is notified so
  that it can inform all the blocking clients whose requested pattern
  matches this tuple to retry the _in_/_rd_ operation.

* `espace_tspatt_srv` maintains the `espace_tspatt` ETS table, which
  contains the tuple patterns that worker processes (clients) are
  blocked on. It accepts two types of requests from
  `espace_tspace_srv`, one for adding unmatched patterns, which, along
  with the client details, are then added to the `espace_tspatt`
  table, and the other for the newly added tuples, which are matched
  against the existing patterns in the table and, if matched, notify
  the blocked clients to retry the `in`/`rd` operation.

## The ETS Tables

The tuples are kept in a pair of ETS tables, `espace_tspace` and
`espace_tspatt`.

The first table, `espace_tspace` has two columns, an integer, and the
tuple itself. All access to the table is handled by a single server,
`espace_tspace_srv`.

For the cases where no matching tuple is found for a given pattern,
and the operation is a blocking one, i.e. `in` or `rd`, a second ETS
table `espace_tspatt` is used to remember the pattern, the client PID
and a unique reference for this specific request. The unique reference
will be used by the `espace` client side function to block on a
receive for that reference.

Each `out` operation, after inserting the new tuple, will notify
`espace_tspatt_srv` of the new tuple. `espace_tspatt_srv` will then
scan the blocking clients whose pattern matches the newly added
tuple. If a match is found, the waiting client is notified and the
client entry is removed from the table.

Once the waiting client receives the notification, it will attempt the
`rd` or `in` operation again. Note that the client is not handed the
tuple, but notified to request it again. This waiting and the
subsequent notification occurs inside the espace client code. If more
than one client have waiting patterns that match the new tuple, then
they may end up competing for it. However, since the ETS table is an
`ordered_set`, the notification will be performed in a first come
(blocked) first served (notified) manner. However, we cannot guarantee
that the first unblocked client will get the tuple! This
non-determinism is part of the original specification.

## The Operation Counters

For each running instance of `espace` a set of counters are
maintained. Each set consists of six counters, one for each `espace`
operation. The counters are incremented whenever the operation is
requested. Note that each `eval` will increment two counters, `eval`
and `out`.

The functions for the operation of the counters are in the
`espace_util` module and have the prefix `opcount_`. However, only two
of them are meant for general use, `opcount_counts/0,1` and
`opcount_reset/0,1`. The former will return a map of the
operation/count pairs, while the latter will reset the counts to zero.

## The persistent term records

In order to provide better visibility of the internal workings of an
`espace` instance, as well as simplifying some of the code, a set of
`persistent_term` records are maintained for each active instance.

The functions for manipulating the terms are in the `espace_pterm`
module. The terms are created during the instance startup, and removed
during the instance shutdown.

The keys are of the form `{espace, Inst :: atom(), Key :: atom()}`,
where `Inst` is the instance name, and `Key` specifies the particular
term. The current keys are listed below:

1. `espace_sup`, `espace_tspace_srv`, `espace_tspatt_srv` and
  `etsmgr_srv` identify the registered server names.  The entry for
  each server is automatically created during the first call of
  `espace_util:inst_to_name/2` for that server.

1. `tspace_tabid` and `tspatt_tabid` contain the ETS table ids.

1. `opcounters` identifies the reference for the `counters` module.

1. `espace_tspace` and `espace_tspatt` identify the table names for
    the `etsmgr` instance.

---
