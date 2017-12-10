-module(knot_hash).
-compile(export_all).

compute_hash(Lengths, HashLength) ->
  compute_hash(Lengths, lists:seq(0, HashLength - 1), 0, 0).

compute_hash([], HashList, _, _) ->
  {FirstTwo, _} = lists:split(2, HashList),
  io:format("Reached the end of Lengths with ~w~n", [HashList]),
  lists:foldl(fun(X, Prod) -> X * Prod end, 1, FirstTwo);
compute_hash([H|T], HashList, CurrentPosition, SkipLength) when H =< length(HashList) ->
  NewHashList  = reverse_sublist(HashList, CurrentPosition, H),
  NewPosition = (H + CurrentPosition + SkipLength) rem length(HashList),
  % io:format("Position ~w -> ~w~n", [CurrentPosition, NewPosition]),
  compute_hash(T, NewHashList, NewPosition, SkipLength + 1).

reverse_sublist(List, Start, Len) ->
  StartIndex = Start,
  LastIndex = Start + Len,
  case LastIndex - length(List) of
    X when X < 0 ->
      {NewFirst, Rest} = lists:split(Start, List),
      {Middle, NewLast} = lists:split(Len, Rest),
      NewMiddle = lists:reverse(Middle),
      io:format("Reversed Portion: ~w~n", [NewMiddle]);
    OverflowLength ->
      {First, Rest} = lists:split(OverflowLength, List),
      {NewMiddle, Last} = lists:split(StartIndex - OverflowLength, Rest),
      Reversed = lists:reverse(Last ++ First),
      io:format("Reversed Portion: ~w~n", [Reversed]),
      {NewLast, NewFirst} = lists:split(Len - OverflowLength, Reversed)
  end,
  io:format("StartIndex: ~w, First: ~w, Middle: ~w, Last: ~w, Combined: ~w~n",
            [StartIndex, NewFirst, NewMiddle, NewLast, NewFirst ++ NewMiddle ++ NewLast]),
  NewFirst ++ NewMiddle ++ NewLast.
