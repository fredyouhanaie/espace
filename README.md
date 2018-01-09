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

The initial implementation was carried out as part of the [Spawnfest
2017](https://spawnfest.github.io/) hackathon. So, due to the short
implementation time (48 hours) it was treated as a proof of concept,
that would be optimized over time.

We are now in the post-Spawnfest phase of the project.

The implementation provides a space for the tuples, currently
`ETS`, along with the six basic operations, `eval`, `out`, `in`,
`inp`, `rd` and `rdp`.

## Current Status

* The project has been developed and tested on a *Linux* system. Using Erlang/OTP 20.1 and rebar3 3.4.7
* No automated test scripts, yet!
* Documentation is work in progress.
* Not an scalable implementation, that will come over time.
* Example programs are being added in the `Examples/` directory.

## To check out the application

* Change to the top level directory of the project
* ensure that you have erlang binaries and rebar3 in your shell path
* build the application
```
$ rebar3 do clean,compile
```
* Start the application
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

## Client API Library

The `espace` module can be used to perform the six operations. The
functions in turn call their corrosponding function in `tspool_srv` or
`wkpool_srv`. These are summarized below:

| Operation | Functions    | Notes |
| :---------| :---------   | :-----|
| `in`      | in(Pattern)  | removes and returns tuple, blocks if pattern does not exist |
| `inp`     | inp(Pattern) | non-blocking version of `in`, returns `nomatch` if pattern does not exists |
| `rd`      | rd(Pattern)  | same as `in`, but does not remove the tuple |
| `rdp`     | rdp(Pattern) | non-blocking version of `rd`, returns `nomatch` if pattern does not exists |
|           |              |       |
| `out`     | out(Tuple)   | adds the tuple to the Pool                                                 |
|           |              |       |
| `eval`    | eval(MFA)    | the function MFA will run within a child process |

### Input Operations

The four input functions result in a call to the `tspool_srv` `gen_server`.

* `in` and `inp` will return a tuple, and remove it from the pool, if a tuple matching the pattern is found.
* `rd` and `rdp` will return a tuple, but *will not* remove it from the pool, if a tuple matching the pattern is found.
* `in` and `rd` will block, if a tuple matching the pattern is not present.
* `inp` and `rdp` will return `nomatch`, if a tuple matching the pattern is not present.
* All four operations, upon finding a match, will return `{Fields,
  Tuple}`, where `Tuple` is the entire matched tuple, and `Fields` is
  a list of terms corresponding to the `'$N'` terms in the pattern, if
  any.

### Output Operation

Currently, `out` will accept any Erlang term and store it in the
pool. In future this may be restricted to be a tuple.

### Eval Operation

`eval` will send a request to the worker supervisor, which in turn
will start a new child process, that will `apply` the `{M, F, A}`
function. `{M, F, A}` can be a single shot functions, or it can be
recursive loop that runs continuously.

### `espace:infile(File)`

Normally, the above operations are called via functions within Erlang
modules, which will need to be compiled when modified.

To make life easier for the `espace` programmer, an include file
mechanism allows one to put simple, `out` and `eval` operations in a
file. Below is an example of such a file, which corresponds to the
`adder1:start/0` function. Below is a copy of the file
`Examples/adder1/adder1.esp`:


```
%% this is the adder example

% start the worker processes
%
{eval, {adder1, test_add2, []}}.
{eval, {adder1, test_sums, []}}.

% initial set of numbers
%
{out, {add, 1, 2}}.
{out, {add, 2, 3}}.
{out, {add, 3, 5}}.

% read more numbers to be added together.
%
{include, "adder1a.esp"}.
```

The above file can be run with `espace:infile("adder1.esp")`,
while in the `Examples/adder1` directory, instead of calling
`adder1:start()`, for example starting from the top level
directory:

```
$ rebar3 shell
... Erlang shell start up messages
1> cd("Examples/adder1").
2> espace:infile("adder1.esp").
... adder output
```

The contents of the file are tuples of the form `{Tag, Term}`, where
`Tag` is one of `eval`, `out` or `include`. The file is read with
`file:consult/1`, so all the rules of that function apply.

The meanings of the three types of tags should, hopefully, be clear to
the reader:

* `eval` will call `espace:eval/1` with the corresponding `Term`,
  which should be of the form `{Mod, Fun, Arg}`
* `out` will call `espace:out/1` with the corresponding `Term`
* `include` will recursively call `espace:infile/1`, where `Term`
  is expected to be a valid filename.

## Organization of the Project Modules

`espace` has been built as an OTP application, `espace_app`. Presently
only one instance of the application can run on a given node.

Once the server application is started, the client applications can use
the `espace` module to perform any of the six operations. In fact
one can kick off the whole process by calling a purpose written bootstrap
function, which can in turn perform `out` and `eval` operations, e.g.

> `espace:eval({Mod, Func, Args})`.

On the server side there are two main components:

- `tspool_srv` is a `gen_server` that manages the TS storage pool. This server will handle the `out`, `in`, `inp`, `rd` and `rdp` requests.
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
