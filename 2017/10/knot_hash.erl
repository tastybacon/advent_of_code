-module(knot_hash).
-export([compute_hash_round/2, hash/0, hash/3, to_hex/1]).

to_hex(List) ->
  list_to_binary(
    lists:flatmap(
      fun(X) -> io_lib:format("~2.16.0B", [X]) end,
      List
     )
   ).

dense_hash(SparseHash) ->
  dense_hash(SparseHash, []).

dense_hash([], Acc) ->
  lists:reverse(Acc);
dense_hash(SparseHash, DenseAcc) ->
  {Current, Rest} = lists:split(16, SparseHash),
  Densified = lists:foldl(fun(X, Acc) -> X bxor Acc end, 0, Current),
  dense_hash(Rest, [Densified | DenseAcc]).

hash() ->
  hash("187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216", 256, 64).

hash(Input, HashLength, Rounds) ->
  hash(Input ++ [17, 31, 73, 47, 23], lists:seq(0, HashLength - 1), Rounds, 0, 0).

hash(_, SparseHash, 0, _, _) ->
  dense_hash(SparseHash);
hash(Input, HashRound, RemainingRounds, CurrentPosition, SkipLength) ->
  {NewHashRound, NewPosition, NewSkipLength} = compute_hash_round(Input, HashRound, CurrentPosition, SkipLength),
  %io:format("Input: ~w~n", [Input]),
  io:format("Rounds: ~w, Position: (~w -> ~w), SkipLength: (~w -> ~w)~n",
            [RemainingRounds, CurrentPosition, NewPosition, SkipLength, NewSkipLength]),
  hash(
    Input,
    NewHashRound,
    RemainingRounds - 1,
    NewPosition,
    NewSkipLength
   ).

compute_hash_round(Lengths, HashLength) ->
  compute_hash_round(Lengths, lists:seq(0, HashLength - 1), 0, 0).

compute_hash_round([], HashList, CurrentPosition, SkipLength) ->
  {HashList, CurrentPosition, SkipLength};
compute_hash_round([H|T], HashList, CurrentPosition, SkipLength) when H =< length(HashList) ->
  NewHashList  = reverse_sublist(HashList, CurrentPosition, H),
  NewPosition = (H + CurrentPosition + SkipLength) rem length(HashList),
  %io:format("Position: ~w -> ~w~n", [CurrentPosition, NewPosition]),
  compute_hash_round(T, NewHashList, NewPosition, SkipLength + 1).

reverse_sublist(List, Start, Len) ->
  StartIndex = Start,
  LastIndex = Start + Len,
  case LastIndex - length(List) of
    X when X < 0 ->
      {NewFirst, Rest} = lists:split(Start, List),
      {Middle, NewLast} = lists:split(Len, Rest),
      NewMiddle = lists:reverse(Middle);
    OverflowLength ->
      {First, Rest} = lists:split(OverflowLength, List),
      {NewMiddle, Last} = lists:split(StartIndex - OverflowLength, Rest),
      Reversed = lists:reverse(Last ++ First),
      {NewLast, NewFirst} = lists:split(Len - OverflowLength, Reversed)
  end,
  NewFirst ++ NewMiddle ++ NewLast.
