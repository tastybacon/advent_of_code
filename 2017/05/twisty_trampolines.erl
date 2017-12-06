-module(twisty_trampolines).
-compile(export_all).

count_steps(List) ->
  Array = array:from_list(List),
  step(0, 0, Array).

step(Position, Acc, Array) when Position >= 0 ->
  CurrentOffset = array:get(Position, Array),
  NewPosition = Position + CurrentOffset,
  StepCount = Acc + 1,
  case NewPosition < array:size(Array) of
    true -> step(NewPosition, StepCount, array:set(Position, CurrentOffset + 1, Array));
    false -> StepCount
  end;
step(_, Acc, _) ->
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
