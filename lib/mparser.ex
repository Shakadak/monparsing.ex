defmodule Mparser do
  @enforce_keys [:parser]
  defstruct [:parser]
  @type t(a) :: %__MODULE__{parser: Monparsing.parser(a)}

  @spec mk(Monparsing.parser(a)) :: t(a) when a: any
  def mk(x), do: %__MODULE__{parser: x}

  @spec run(Monparsing.parser(a), String.t) :: [{a, String.t}] when a: any
  def run(%__MODULE__{parser: f}, x), do: f.(x)

  defmacro delay(body) do
    quote do
      Mparser.mk(fn inp -> Mparser.run(unquote(body), inp) end)
    end
  end

  def inspect_fun(fun) do
    fun_info = :erlang.fun_info(fun)
    #_ = IO.inspect(fun_info)
    case Keyword.fetch!(fun_info, :type) do
      :external -> IO.inspect(fun)
      :local ->
        case Keyword.fetch!(fun_info, :env) do
          {:env, [{_, _, _, _, _, abs}]} ->
            str = :erl_pp.expr({:fun, 1, {:clauses, abs}})
            _ = IO.puts(str)
            fun

          [f] when is_function(f) ->
            #inspect_fun(f)
            #fun
            IO.inspect(fun, label: "bad match in inspect_fun")

          [] ->
           IO.inspect(fun)

          xs when is_list(xs) ->
            IO.inspect(fun, label: "bad match in inspect_fun")
        end
    end
    fun
  end

  def test do
    for(
      x <- Monparsing.digit,
      y <- Monparsing.digit,
      y == ?2,
      z <- Monparsing.digit(),
      into: Monparsing.zero(),
      do: <<x, y, z>>
    )
    |> run("1234")
  end

  defimpl Collectable do
    def into(%Mparser{}) do
      collector = fn
        _, :halt ->
          "welp ¯\_(ツ)_/¯"

        parser, :done ->
          parser

        _p, {:cont, v} -> Monparsing.result(v)
      end

      initial_acc = Monparsing.zero()

      {initial_acc, collector}
    end
  end

  defimpl Enumerable do
    def count(_function), do: {:error, __MODULE__}
    def member?(_function, _value), do: {:error, __MODULE__}
    def slice(_function), do: {:error, __MODULE__}

    def reduce(%Mparser{}, {:halt, acc}, _fun) do
      raise("#{inspect(__MODULE__)}.reduce(parser, {:halt, acc}, _fun)")
      {:halted, acc}
    end
    def reduce(%Mparser{} = mp, {:suspend, acc}, fun) do
      raise("#{inspect(__MODULE__)}.reduce(parser, {:suspend, acc}, _fun)")
      {:suspended, acc, &reduce(mp, &1, fun)}
    end

    def reduce(%Mparser{} = mp, {:cont, acc}, f) do
      ret = Monparsing.bind(mp, fn x ->
        {:cont, v} = f.(x, acc)
        v
      end)

      {:done, ret}
    end
  end
end
