# Dijkstra's Shortest Path Algorithm

This is a first attempt at implementing Dijkstra's shortest path
algorithm using `espace`. See the
[Wikipedia](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
page, and references thereof, for the details of the algorithm.

Given a directed graph, with `costs` associated with each edge
connecting two vertices, the algorithm allows for the compuation of
the path with the least cost between any two vertices.

In its current form, the implementation will allow for the shortest
path between any pair of vertices to be determined concurrently along
with other pairs, but for a single graph only.

There are two example graphs in the two files, `dijkstra-1.esp` and
`dijkstra-2.esp`.

## The tuple types (data structure)

The input to the algorithm is a single tuple defining the entire
directed graph, e.g.

```
{graph, [
         {s, [{a, 2}, {b, 1}] },
         {a, [{s, 3}, {b, 4}, {c, 8}] },
         {b, [{s, 4}, {a, 2}, {d, 2}] },
         {c, [{a, 2}, {b, 7}, {d, 4}] },
         {d, [{b, 1}, {c, 11}, {t, 5}] },
         {t, [{c, 3}, {d, 5}] }
        ]
}
```

From the above tuple we produce a set of individual tuples, one per
vertex, using the `gen_vertex/0` worker. Each tuple shows the
neighbours of each vertex along with the cost of that edge, e.g.

```
{vertex, s, [{a, 2}, {b, 1}] }
{vertex, a, [{s, 3}, {b, 4}, {c, 8}] }
{vertex, b, [{s, 4}, {a, 2}, {d, 2}] }
...
```

To keep track of the shortest paths to individual vertices, for a
specific source-destination computation, we maintain a set of
`vertex_status` tuples. Each tuple consists of the vertex id, the
source-destination computation it is part of, the predecessor of the
shortest path computed so far, and the cost of that path. The latter
two are initialized to `none` and `infinity`, except for the source
vertex, which is initialized to the source id itself and `0`. Below is
an example of the for computing the shortest path from `s` to `t`:

```
{vertex_status, s, {s, t}, s, 0}
{vertex_status, a, {s, t}, none, infinity}
{vertex_status, b, {s, t}, s, 1}
...
```

We move (concurrently) forward with the computation by generating
`nexthop` tuples, which in turn are picked up by one or more
`nexthop_worker` worker processes. The computation is started by
generating `nexthop` tuples for the source vertex. Below are the
initial tuples generated at the start of our example:

```
{nexthop, {s, t}, s, a, 2}
{nexthop, {s, t}, s, b, 1}
```

When the above two tuples are processed by `nexthop_worker` processes,
in some arbitrary order, the `vertex_status` tuples for `a` and `b`
will be updated, and the following set of tuples will be generated:

```
{nexthop, {s, t}, a, s, 5}
{nexthop, {s, t}, a, b, 6}
{nexthop, {s, t}, a, c, 8}
{nexthop, {s, t}, b, s, 5}
{nexthop, {s, t}, b, a, 4}
{nexthop, {s, t}, b, d, 3}
```

For the above only the third and sixth tuples will lead to
`vertex_status` updates, for `c` and `d` respectively, and further
`nexthop` tuples.

The above process will continue and we will eventually end up with a
stable set of `vertex_status` tuples and no `nexthop` ones. We now
need a way of recognising that this steady state has been reached.

This is achieved by maintaining the list of vertices that are to be
visited in a `visit_list` tuple.

This list is initially empty. Every time we generate a `nexthop`
tuple, via `gen_nexthop/2`, we add the target vertex id to the list.
Note that the same vertex may appear in the `visit_list` multiple
times.

Whenever a vertex is processed via a `nexthop` worker, a `visited`
tuple is generated, which is picked up by the `wait_for_completion/1`
worker.

When a `visited` tuple is picked up, the corresponding vertex is
removed from the `visit_list`. If the list is empty, then the
`shortest_path` tuple is created by tracing back from the destination
`vertex_status` until we reach the source `vertex_status`. For the
above example this will be

```
{shortest_path, {s,t}, [s,b,d,t], 8}
```

Once the above result is produced, all the remaing `vertex_status`
tuples are removed.

To try out this example, assuming the application has been built with
`rebar3`:

```
$ rebar3 shell
> cd("Examples/dijkstra").
> espace_cli:infile("dijkstra-1.esp").
> espace_cli:rdp({shortest_path, {s, t}, '_', '_'}).
```

While in the shell, and while the `graph` and `vertex` tuples are
present, you can initiate a shortest path computation for any pair of
vertices, e.g.

```
> dijkstra:compute_sp({t, s}).
> espace_cli:rdp({shortest_path, {t, s}, '_', '_'}).
```

## Further work

The current implementation works well, but there is plenty of room for
improvement. Some of these improvements are listed below:

- Allow for multiple graphs to be stored in the tuple space.
  - This would require all tuples to carry an additional graph id tag.
- Add function(s) to generate the single-source shortest paths (SSSP)
  - also single-destination shortest path (SDSP)
- Add function to generate the shortest path tree from the SSSP tuples.
- Add function(s) to remove all tuples related to a graph.
- etc.
