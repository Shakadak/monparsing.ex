defmodule Monparsing do
  @moduledoc """
  See http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf for more informations
  """

  require Mparser

  def trace do
    :dbg.start()
    :dbg.tracer()
    :dbg.tpl(__MODULE__, :cx)
    #:dbg.tpl(Collectable.Mparser, :cx)
    #:dbg.tpl(Enumerable.Mparser, :cx)
    #:dbg.tp(Enumerable, :cx)
    #:dbg.tp(Enum, :cx)
    :dbg.p(:all, :c)
  end

  # 2.1 The type of parsers

  @type parser(a) :: Mparser.t((String.t -> [{a, String.t}]))

  # 2.2 Primitive parsers

  @spec result(any) :: parser(any)
  def result(v), do: Mparser.mk(fn inp -> [{v, inp}] end)

  @spec zero :: parser(any)
  def zero, do: Mparser.mk(fn _inp -> [] end)

  @spec item :: parser(char)
  def item, do: Mparser.mk(fn inp ->
    case inp do
      "" -> []
      <<x::utf8, xs::binary>> -> [{x, xs}]
    end
  end)

  # 2.3 Parser combinators

  @spec seq(parser(a), parser(b)) :: parser({a, b}) when a: any, b: any
  def seq(p, q), do: Mparser.mk(fn inp ->
    for(
      {v, inp1} <- Mparser.run(p, inp),
      {w, inp2} <- Mparser.run(q, inp1),
      do: {{v, w}, inp2}
    )
  end)

  @spec bind(parser(a), (a -> parser(b))) :: parser(b) when a: any, b: any
  def bind(p, f) do
    #IO.inspect(p, label: "bind(p, _)")
    Mparser.mk(fn inp ->
      Enum.concat(for {v, inp1} <- Mparser.run(p, inp), do: Mparser.run(f.(v), inp1))
    end)
  end

  @spec seq_bis(parser(a), parser(b)) :: parser({a, b}) when a: any, b: any
  def seq_bis(p, q) do
    p |> bind(fn x ->
      q |> bind(fn y ->
        result({x, y})
      end)
    end)
  end

  @spec seq_bis(parser(a), parser(b)) :: parser({a, b}) when a: any, b: any
  def seq_ter(p, q), do: for(v <- p, w <- q, do: {v, w}, into: zero())

  @spec sat((char -> boolean)) :: parser(char)
  def sat(p), do: item() |> bind(fn x ->
    if p.(x) do result(x) else zero() end
  end)

  @spec char(char) :: parser(char)
  def char(x), do: sat(fn y -> y == x end)

  @spec digit :: parser(char)
  def digit, do: sat(fn x -> ?0 <= x and x <= ?9 end)

  @spec lower :: parser(char)
  def lower, do: sat(fn x -> ?a <= x and x <= ?z end)

  @spec digit :: parser(char)
  def upper, do: sat(fn x -> ?A <= x and x <= ?Z end)

  @spec plus(parser(a), parser(a)) :: parser(a) when a: any
  def plus(p, q), do: Mparser.mk(fn inp -> Mparser.run(p, inp) ++ Mparser.run(q, inp) end)

  @spec letter :: parser(char)
  def letter, do: lower() |> plus(upper())

  @spec alphanum :: parser(char)
  def alphanum, do: letter() |> plus(digit())

  @spec word :: parser(String.t)
  def word do
    neWord =
      letter() |> bind(fn x ->
        word() |> bind(fn xs ->
          result(<<x, xs::binary>>)
        end)
      end)
    neWord |> plus(result(""))
  end

  # 3.2 Monad comprehension syntax

  @spec string(String.t) :: parser(String.t)
  def string(""), do: result("")
  def string(<<x::utf8, xs::binary>>) do
    for _ <- char(x), _ <- string(xs), do: <<x::utf8, xs::binary>>, into: zero()
  end

  @spec string_bis(String.t) :: parser(String.t)
  def string_bis(""), do: result("")
  def string_bis(<<x::utf8, xs::binary>>) do
    char(x) |> bind(fn _ ->
      string_bis(xs) |> bind(fn _ ->
        result(<<x::utf8, xs::binary>>)
      end)
    end)
  end

  @spec sat_bis((char -> boolean)) :: parser(char)
  def sat_bis(p), do: for x <- item(), p.(x), do: x, into: zero()

  # 4 Combinators for repetition
  # 4.1 Simple repetition

  @spec word_bis :: parser(String.t)
  def word_bis do
    for(
      x <- letter(),
      xs <- word_bis(),
      do: <<x::utf8, xs::binary>>,
      into: zero()
    )
    |> plus(result(""))
  end

  @spec many(parser(a)) :: parser([a]) when a: any
  def many(p) do
    for(x <- p, xs <- many(p), do: [x | xs], into: zero())
    |> plus(result([]))
  end

  @spec ident :: parser(String.t)
  def ident, do: for x <- lower(), xs <- many(alphanum()), do: <<x::utf8, to_string(xs)::binary>>, into: zero()

  @spec many1(parser(a)) :: parser([a, ...]) when a: any
  def many1(p), do: for x <- p, xs <- many(p), do: [x | xs], into: zero()

  @spec nat :: parser(non_neg_integer)
  def nat, do: for xs <- many1(digit()), do: List.to_integer(xs), into: zero()

  @spec int :: parser(integer)
  def int do
    for(_ <- char(?-), n <- nat(), do: -n, into: zero())
    |> plus(nat())
  end

  @spec int_bis :: parser(integer)
  def int_bis do
    op = for(_ <- char(?-), do: &-/1, into: zero()) |> plus(result(&Function.identity/1))
    for f <- op, n <- nat(), do: f.(n), into: zero()
  end

  # 4.2 Repetition with separators

  @spec ints :: parser([integer, ...])
  def ints do
    for(
      _ <- char(?[),
      n <- int(),
      ns <- many(for _ <- char(?,), x <- int(), do: x, into: zero()),
      _ <- char(?]),
      do: [n | ns],
      into: zero()
    )
  end

  @spec sepby1(parser(a), parser(b)) :: parser([a, ...]) when a: any, b: any
  def sepby1(p, sep) do
    for(
      x <- p,
      xs <- many(for _ <- sep, y <- p, do: y, into: zero()),
      do: [x | xs],
      into: zero()
    )
  end

  @spec ints_bis :: parser([integer, ...])
  def ints_bis do
    for(
      _ <- char(?[),
      ns <- int() |> sepby1(char(?,)),
      _ <- char(?]),
      do: ns,
      into: zero()
    )
  end

  @spec bracket(parser(a), parser(b), parser(c)) :: parser(b) when a: any, b: any, c: any
  def bracket(open, p, close), do: for(_ <- open, x <- p, _ <- close, do: x, into: zero())

  @spec ints_ter :: parser([integer, ...])
  def ints_ter, do: bracket(char(?[), int() |> sepby1(char(?,)), char(?]))

  @spec sepby(parser(a), parser(b)) :: parser([a]) when a: any, b: any
  def sepby(p, sep), do: p |> sepby1(sep) |> plus(result([]))

  # 4.3 Repetition with meaningful separators

  @spec expr :: parser(integer)
  @spec addop :: parser((integer, integer -> integer))
  @spec factor :: parser(integer)

  #def expr do
  #  for(
  #    x <- expr(),
  #    f <- addop(),
  #    y <- factor(),
  #    do: f.(x, y),
  #    into: zero()
  #  )
  #  |> plus(factor())
  #end

  def addop do
    for(_ <- char(?+), do: &+/2, into: zero())
    |> plus(for _ <- char(?-), do: &-/2, into: zero())
  end

  def factor do
    nat()
    # Adding some laziness. Delaying `expr/0` evaluation.
    |> plus(Mparser.mk(fn inp -> bracket(char(?(), expr(), char(?))) |> Mparser.run(inp) end))
  end

  def expr do
    for(
      x <- factor(),
      fys <- many(for f <- addop(), y <- factor(), do: {f, y}, into: zero()),
      do: Enum.reduce(fys, x, fn {op, y}, acc -> op.(acc, y) end),
      into: zero()
    )
  end

  @spec chainl1(parser(a), parser((a, a -> a))) :: parser(a) when a: any
  def chainl1(p, op) do
    for(
      x <- p,
      fys <- many(for f <- op, y <- p, do: {f, y}, into: zero()),
      do: Enum.reduce(fys, x, fn {f, y}, acc -> f.(acc, y) end),
      into: zero()
    )
  end

  @spec expr_bis :: parser(integer)
  def expr_bis do
    factor_bis() |> chainl1(addop())
  end

  @spec factor_bis :: parser(integer)
  def factor_bis do
    nat() |> plus(Mparser.mk(fn inp -> bracket(char(?(), expr_bis(), char(?))) |> Mparser.run(inp) end))
  end

  @spec ops([{parser(a), b}]) :: parser(b) when a: any, b: any
  def ops(xs) do
    for({p, op} <- xs, do: for(_ <- p, do: op, into: zero()))
    |> Enum.reduce(&plus/2)
  end

  @spec expr_ter :: parser(integer)
  @spec addop_ter :: parser((integer, integer -> integer))
  @spec factor_ter :: parser(integer)

  def expr_ter do
    factor_ter() |> chainl1(addop_ter())
  end

  def addop_ter do
    ops([{char(?+), &+/2}, {char(?-), &-/2}])
  end

  def factor_ter do
    nat() |> plus(Mparser.mk(fn inp -> bracket(char(?(), expr_ter(), char(?))) |> Mparser.run(inp) end))
  end

  @spec chainl1_4(parser(a), parser((a, a -> a))) :: parser(a) when a: any
  def chainl1_4(p, op), do: p |> bind(chainl1_4_rest(p, op))
  @doc false
  def chainl1_4_rest(p, op) do
    fn x ->
      q = op |> bind(fn f ->
        p |> bind(fn y ->
          chainl1_4_rest(p, op).(f.(x, y))
        end)
      end)
      q |> plus(result(x))
    end
  end

  @spec nat_4 :: parser(integer)
  def nat_4 do
    op = fn m, n -> 10 * m + n end
    for(x <- digit(), do: x - ?0, into: zero())
    |> chainl1_4(result(op))
  end

  @spec chainr1(parser(a), parser((a, a -> a))) :: parser(a) when a: any
  def chainr1(p, op) do
    p |> bind(fn x ->
      for(f <- op, y <- p |> chainr1(op), do: f.(x, y), into: zero())
      |> plus(result(x))
    end)
  end

  @spec expr_4 :: parser(integer)
  @spec term_4 :: parser(integer)
  @spec factor_4 :: parser(integer)
  @spec addop_4 :: parser((integer, integer -> integer))
  @spec expop_4 :: parser((integer, integer -> integer))

  def expr_4, do: term_4() |> chainl1_4(addop_4())
  def term_4, do: factor_4() |> chainr1(expop_4())
  def factor_4, do: nat_4() |> plus(Mparser.mk(fn inp -> bracket(char(?(), expr_4(), char(?))) |> Mparser.run(inp) end))
  def addop_4, do: ops([{char(?+), &+/2}, {char(?-), &-/2}])
  def expop_4, do: ops([{char(?^), &Integer.pow/2}])

  @spec chainl(parser(a), parser(((a, a) -> a)), a) :: parser(a) when a: any
  @spec chainr(parser(a), parser(((a, a) -> a)), a) :: parser(a) when a: any

  def chainl(p, op, v), do: p |> chainl1(op) |> plus(result(v))
  def chainr(p, op, v), do: p |> chainr1(op) |> plus(result(v))

  # 5.1 Left factoring

  @spec eval :: parser(integer)
  def eval do
    add = for x <- nat(), _ <- char(?+), y <- nat(), do: x + y, into: zero()
    sub = for x <- nat(), _ <- char(?-), y <- nat(), do: x - y, into: zero()
    add |> plus(sub)
  end

  @spec eval_bis :: parser(integer)
  def eval_bis do
    add = fn x -> for _ <- char(?+), y <- nat(), do: x + y, into: zero() end
    sub = fn x -> for _ <- char(?-), y <- nat(), do: x - y, into: zero() end
    for x <- nat(), v <- plus(add.(x), sub.(x)), do: v, into: zero()
  end

  @spec eval_ter :: parser(integer)
  def eval_ter do
    for(
      x <- nat(),
      f <- ops([{char(?+), &+/2}, {char(?-), &-/2}]),
      y <- nat(),
      do: f.(x, y),
      into: zero()
    )
  end

  # 5.3 Limiting the number of results

  # Not as useful as in a lazy lang,
  # could move parser to use a stream instead of a list in its type.
  @spec first(parser(a)) :: parser(a) when a: any
  def first(p) do
    Mparser.mk(fn inp ->
      case Mparser.run(p, inp) do
        [] -> []
        [x | _] -> [x]
      end
    end)
  end

  @spec plusf_(parser(a), parser(a)) :: parser(a) when a: any
  def plusf_(p, q), do: first(plus(p, q))

  @spec plusf(parser(a), parser(a)) :: parser(a) when a: any
  def plusf(p, q) do
    first(Mparser.mk(fn inp ->
      case Mparser.run(p, inp) do
        [] -> Mparser.run(q, inp)
        [_ | _] = xs -> xs
      end
    end))
  end

  # 6.1 White-space, comments, and keywords

  @spec spaces :: parser({})
  def spaces, do: for(_ <- many1(sat(fn x -> x in [?\s, ?\n, ?\t] end)), do: {}, into: zero())

  @spec comment :: parser({})
  def comment, do: for(_ <- string("--"), _ <- many(sat(fn x -> x != ?\n end)), do: {}, into: zero())

  @spec comment_ml :: parser({})
  def comment_ml do
    inner =
      Mparser.delay(comment_ml())
      |> plus(item())
      |> many()

    first(for _ <- bracket(string("{-"), inner, string("-}")), do: {}, into: zero())
  end

  @spec junk :: parser({})
  def junk, do: for(_ <- many(spaces() |> plusf(comment())), do: {}, into: zero())

  @spec parse(parser(a)) :: parser(a) when a: any
  def parse(p), do: for(_ <- junk(), v <- p, do: v, into: zero())

  @spec token(parser(a)) :: parser(a) when a: any
  def token(p), do: for(v <- p, _ <- junk(), do: v, into: zero())

  @spec natural :: parser(integer)
  def natural, do: token(nat())

  @spec integer :: parser(integer)
  def integer, do: token(int())

  @spec symbol(String.t) :: parser(String.t)
  def symbol(bin), do: token(string(bin))

  @spec identifier([String.t]) :: parser(String.t)
  def identifier(ks), do: token(for x <- ident(), x not in ks, do: x, into: zero())
end
