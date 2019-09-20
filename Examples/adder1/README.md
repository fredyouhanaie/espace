# Example - adder1

This is a very simple example of two worker processes where one waits
for tuples with the tag `add` and a pair of numbers, e.g. `{add,
11, 22}`, removes it from the tuple space, and outputs their sum as
another tuple, i.e. `{sum, 11, 22, 33}`.

The second worker waits for the `sum`-tagged tuple, removes it from
the tuple space and writes it to the console.

The main example is `adder1.erl`, with the Elixir equivalent `adder2.ex`.

## To try the Erlang example

(_Note: Another method of running this example can be seen on the main
project README_)

```
$ erl -pa ../../_build/default/lib/espace/ebin -pa ../../_build/default/lib/etsmgr/ebin
> c(adder1).
> espace:start().
> espace:infile("adder1.esp").
```

The set of number pairs listed in the two espace input files,
`adder1.esp` and `adder1a.esp`, should be summed and printed on the
console.

## To try out the Elixir example

This should produce the same result as the Erlang one above.

```
$ iex -pa ../../_build/default/lib/espace/ebin -pa ../../_build/default/lib/etsmgr/ebin
> c("adder2.ex")
> :espace.start()
> :espace.infile("adder2.esp")
```

It was necessary to convert `adder1.esp` to `adder2.esp` since we are
refering to Elixir worker functions. However, since `adder1a.esp` does
not refer to any Erlang/Elixir functions, it can be used by both
sides.
