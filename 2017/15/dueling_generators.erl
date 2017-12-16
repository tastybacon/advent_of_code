-module(dueling_generators).
-compile(export_all).

new_generator(InitialValue, Multiplier, Divisor) ->
  new_generator(InitialValue, Multiplier, Divisor, fun(_) -> true end).

new_generator(InitialValue, Multiplier, Divisor, Predicate) ->
  {generator, InitialValue, Multiplier, Divisor, Predicate}.

generator_value({generator, Value, _, _, _}) ->
  Value.

step({generator, PreviousValue, Multiplier, Divisor, Predicate}) ->
  NextValue = PreviousValue * Multiplier rem Divisor,
  NextGenerator = {generator, NextValue, Multiplier, Divisor, Predicate},
  case Predicate(NextValue) of
    true -> NextGenerator;
    false -> step(NextGenerator)
  end.

judge(Rounds, Generators) ->
  judge(Rounds, Generators, 0).

judge(0, _, Count) ->
  Count;
judge(Rounds, Generators, Count) ->
  NewGenerators = lists:map(fun step/1, Generators),
  Results = lists:map(
              fun({generator, V, _, _, _}) -> V band 16#FFFF end,
              NewGenerators),
  NewCount = case sets:size(sets:from_list(Results)) of
               1 -> Count + 1;
               _ -> Count
             end,
  judge(Rounds - 1, NewGenerators, NewCount).
