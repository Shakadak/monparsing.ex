for _ <- MP.mk(Monparsing.digit), y <- MP.mk(Monparsing.digit), z <- MP.mk(Monparsing.digit()), into: %{}, do: <<y, z>>

MP.mk(Monparsing.digit()) |> Enum.reduce(_@3, fn (y, _@3) ->
  MP.mk(Monparsing.digit()) |> Enum.reduce(_@3, fn(z, _@3) ->
    [<<y/integer, z/integer>> | _@3]
  end)
end)

for y <- MP.mk(Monparsing.digit), z <- MP.mk(Monparsing.digit()), do: <<y, z>>

digit |> bind(fn y ->
  digit |> bind(fn z ->
    result(<<y, z>>)
  end)
end)

MP.mk(Monparsing.digit()) |> Enum.reduce(_@3, fn (y, _@3) ->
  MP.mk(Monparsing.digit()) |> Enum.reduce(_@3, fn(z, _@3) ->
    [<<y/integer, z/integer>> | _@3]
  end)
end)

digit |> Enum.reduce(acc, fn (y, acc) ->
  digit |> Enum.reduce(acc, fn(z, acc) ->
    [<<y/integer, z/integer>> | acc]
  end)
end)

#####################################################################################

for(
  x <- MP.mk(Monparsing.digit),
  y <- MP.mk(Monparsing.digit),
  z <- MP.mk(Monparsing.digit()),
  into: mk(nil),
  do: <<x, y, z>>
)

{initial_acc, collector} = Collectable.into(mk(nil))
try do
  MP.mk(Monparsing.digit()) |> Enum.reduce(initial_acc, fn (x, acc) ->
    MP.mk(Monparsing.digit()) |> Enum.reduce(acc, fn (y, acc) ->
      MP.mk(Monparsing.digit()) |> Enum.reduce(acc, fn (z, acc) ->
        collector(acc,
          {:cont,
            <<x/integer,
            y/integer,
            z/integer>>})
      end)
    end)
  end)
else
  x -> collector(x, :done)
catch
  class, reason ->
    collector(initial_acc, :halt),
    :erlang.raise(class, reason, __STACKTRACE__)
end
