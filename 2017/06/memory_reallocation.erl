-module(memory_reallocation).
-export([reallocate/1, test_reallocate_part_1/0]).

reallocate(Blocks) ->
  reallocate(Blocks, 0, []).
reallocate(Blocks, Count, PreviousStates) ->
  case lists:member(Blocks, PreviousStates) of
    true -> CycleLength = index_of(Blocks, PreviousStates) + 1,
      {ok, Count, Blocks, CycleLength};
    false -> NewBlocks = step(Blocks),
             reallocate(NewBlocks, Count + 1, [Blocks | PreviousStates])
  end.

step(Blocks) ->
  {ok, MaxValue, MaxIndex} = max_with_index(Blocks),
  Array = array:set(MaxIndex, 0, array:from_list(Blocks)),
  step(Array, MaxValue, MaxIndex, 0).
step(Array, 0, _, _) ->
  array:to_list(Array);
step(Array, Remaining, ZeroPoint, RelativeIndex) ->
  CurrentIndex = (RelativeIndex + 1 + ZeroPoint) rem array:size(Array),
  CurrentValue = array:get(CurrentIndex, Array),
  step(
    array:set(CurrentIndex, CurrentValue + 1, Array),
    Remaining - 1,
    ZeroPoint,
    RelativeIndex + 1
   ).

index_of(Element, List) ->
  index_of(Element, List, 0).
index_of(Element, [Element | _], Index) ->
  Index;
index_of(Element, [_ | T], Index) ->
  index_of(Element, T, Index + 1).

max_with_index([]) ->
  {err};
max_with_index(X) ->
  max_with_index(X, none, none, none).
max_with_index([H | T], none, none, none) ->
  max_with_index(T, 0, H, 0);
max_with_index([], _, Previous, MaxIndex) ->
  {ok, Previous, MaxIndex};
max_with_index([H|T], Index, Previous, _) when H > Previous ->
  CurrentIndex = Index + 1,
  max_with_index(T, CurrentIndex, H, CurrentIndex);
max_with_index([_|T], Index, Previous, MaxIndex) ->
  max_with_index(T, Index + 1, Previous, MaxIndex).

test_reallocate_part_1() ->
  TestInput = [0, 2, 7, 0],
  ExpectedFinalState = [2, 4, 1, 2],
  ExpectedSteps = 5,
  {ok, ActualSteps, ActualFinalState} = reallocate(TestInput),
  io:format(
    "Input: ~w | Expected results: ~w : ~w | Actual: ~w : ~w~n",
    [TestInput, ExpectedSteps, ExpectedFinalState, ActualSteps, ActualFinalState]
   ),
  ExpectedSteps == ActualSteps.
