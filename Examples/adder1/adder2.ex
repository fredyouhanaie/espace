## This is the Elixir equivalent of adder1.erl.
##

defmodule Adder2 do

  def start() do
    :code.add_patha('../../_build/default/lib/espace/ebin')
    :code.add_patha('../../_build/default/lib/etsmgr/ebin')

    :espace.start()

    :espace.worker({__MODULE__, :add, []})
    :espace.worker({__MODULE__, :sum, []})

    :espace.out({:add, 1, 2})
    :espace.out({:add, 2, 3})
    :espace.out({:add, 3, 5})
  end

  def stop(), do: :espace.stop()

  def add() do
    {[a, b], _} = :espace.in({:add, :"$1", :"$2"})
    :espace.out({:sum, a, b, a+b})
    add()
  end

  def sum() do
    {[x, y, sum], _} = :espace.in({:sum, :"$1", :"$2", :"$3"})
    :io.format("~n~p: ~p + ~p = ~p.~n", [:test_sums, x, y, sum])
    sum()
  end

end
