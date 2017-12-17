-module(spinlock).
-compile(export_all).

spin(Step) ->
  spin(Step, 2017).

spin(Step, LastValue) ->
  spin(Step, LastValue + 1, [0], 0, 1).

spin(_, LastValue, Buffer, _, LastValue) ->
  Buffer;
spin(Step, LastValue, Buffer, CurrentPosition, Value) ->
  BufferSize = length(Buffer),
  NewIndex = ((CurrentPosition + Step) rem BufferSize) + 1,
  BufferLeft = lists:sublist(Buffer, NewIndex),
  BufferRight = lists:nthtail(NewIndex, Buffer),
  spin(Step, LastValue, BufferLeft ++ [Value] ++ BufferRight, NewIndex, Value + 1).

value_after(_, []) ->
  notfound;
value_after(GivenValue, [GivenValue|T]) ->
  case T of
    [] -> none;
    [Value|_] -> Value
  end;
value_after(GivenValue, [_|T]) ->
  value_after(GivenValue, T).

after_zero(Step, LastValue) ->
  after_zero(Step, LastValue + 1, 1, 2, 1, 1).

after_zero(_, LastValue, AfterZero, _, _, LastValue) ->
  AfterZero;
after_zero(
  Step,
  LastValue,
  AfterZero,
  BufferSize,
  CurrentPosition,
  CurrentValue) ->
  NewIndex = ((CurrentPosition + Step) rem BufferSize) + 1,
  NewAfterZero = case NewIndex of
                   1 -> CurrentValue + 1;
                   _ -> AfterZero
                 end,
  after_zero(Step, LastValue, NewAfterZero, BufferSize + 1, NewIndex, CurrentValue + 1).
