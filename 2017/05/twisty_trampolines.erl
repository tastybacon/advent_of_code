-module(twisty_trampolines).
-export(
   [
    count_steps/1,
    count_steps/2,
    read_jump_list/1,
    part_1_offset/1,
    part_2_offset/1
   ]
  ).

count_steps(List) ->
  count_steps(List, fun part_1_offset/1).
count_steps(List, OffsetFunction) ->
  Array = array:from_list(List),
  step(0, 0, Array, OffsetFunction).

step(Position, Acc, Array, OffsetFunction) when Position >= 0 ->
  CurrentOffset = array:get(Position, Array),
  NewPosition = Position + CurrentOffset,
  StepCount = Acc + 1,
  case NewPosition < array:size(Array) of
    true ->
      NewOffset = OffsetFunction(CurrentOffset),
      step(NewPosition, StepCount, array:set(Position, NewOffset, Array), OffsetFunction);
    false -> StepCount
  end;
step(_, Acc, _, _) ->
  Acc + 1.

read_jump_list(Filename) ->
  {ok, Data} = file:read_file(Filename),
  lists:map(
    fun read_list_item/1,
    lists:filter(
      fun(X) -> X /= <<>> end,
      binary:split(Data, [<<"\n">>], [global])
     )
   ).

read_list_item(RawItem) ->
  list_to_integer(binary_to_list(RawItem)).

part_1_offset(CurrentOffset) ->
  CurrentOffset + 1.

part_2_offset(CurrentOffset) when CurrentOffset >= 3 ->
  CurrentOffset - 1;
part_2_offset(CurrentOffset) ->
  CurrentOffset + 1.
