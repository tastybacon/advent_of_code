-module(disk_defragmentation).
-compile(export_all).

% TODO: Checkout erlang's digraph module and see if it can help us here

disk_state(Input) ->
  disk_state(Input, [], 0, 128).

disk_state(_, State, Max, Max) ->
  lists:reverse(State);
disk_state(Input, State, Counter, Max) ->
  NewState = knot_hash:hash(Input ++ "-" ++ integer_to_list(Counter), 256, 64),
  disk_state(Input, [NewState|State], Counter + 1, Max).

count_bits(List) ->
  count_bits(List, 0).

count_bits([], Count) ->
  Count;
count_bits([H|T], Count) ->
  count_bits(T, bits_in_integer(H) + Count).

bits_in_integer(Int) when Int >= 0, Int < 256 ->
  (Int bsr 0) band 1 +
  (Int bsr 1) band 1 +
  (Int bsr 2) band 1 +
  (Int bsr 3) band 1 +
  (Int bsr 4) band 1 +
  (Int bsr 5) band 1 +
  (Int bsr 6) band 1 +
  (Int bsr 7) band 1.

increment_position({127, Y}) ->
  {0, Y + 1};
increment_position({X, Y}) ->
  {X + 1, Y}.

count_regions(DiskState) ->
  BitSet = state_to_set(DiskState),
  count_regions(BitSet, {0, 0}, 0).

count_regions(BitSet, Position, Count) ->
  NewPosition = increment_position(Position),
  case sets:size(BitSet) /= 0 of
    true ->
      case sets:is_element(Position, BitSet) of
        true ->
          NewSet = remove_grouped_positions(Position, BitSet),
          count_regions(NewSet, NewPosition, Count + 1);
        false ->
          count_regions(BitSet, NewPosition, Count)
      end;
    false ->
      Count
  end.

remove_grouped_positions(StartingPosition, BitSet) ->
  Group = find_group(StartingPosition, BitSet),
  sets:subtract(BitSet, Group).

find_group(StartingPosition, BitSet) ->
  find_group([StartingPosition], BitSet, sets:new(), sets:new()).

find_group([], _, _, Grouped) ->
  Grouped;
find_group([CurrentPosition|T], BitSet, Visited, Grouped) ->
  Adjacent = adjacent_points(CurrentPosition),
  Unvisited = lists:filter(
                fun (X) -> not sets:is_element(X, Visited) end,
                Adjacent
               ),
  case sets:is_element(CurrentPosition, BitSet) of
    true ->
      NewGrouped = sets:add_element(CurrentPosition, Grouped),
      NewPositions = T ++ Unvisited;
    false ->
      NewGrouped = Grouped,
      NewPositions = T
  end,
  find_group(
    sets:to_list(sets:from_list(NewPositions)),
    BitSet,
    sets:add_element(CurrentPosition,Visited),
    NewGrouped
   ).

adjacent_points({X, Y}) ->
  [
   {X - 1, Y},
   {X + 1, Y},
   {X, Y - 1},
   {X, Y + 1}
  ].

state_to_set(DiskState) ->
  state_to_set(DiskState, sets:new(), 0).

state_to_set([], Set, _) ->
  Set;
state_to_set([H|T], Set, Index) ->
  state_to_set(T, sets:union(disk_row_to_set(H, Index), Set), Index + 1).

disk_row_to_set(Row, Index) ->
  disk_row_to_set(Row, sets:new(), 0, Index).

disk_row_to_set([], RowSet, _, _) ->
  RowSet;
disk_row_to_set([H|T], RowSet, Index, RowIndex) ->
  OneIndexes = lists:map(
                 fun(X) -> {X + Index * 8, RowIndex} end,
                 indexes_of_bits(H)
                ),
  ItemSet = sets:from_list(OneIndexes),
  disk_row_to_set(T, sets:union(ItemSet, RowSet), Index + 1, RowIndex).

indexes_of_bits(Int) when Int >= 0, Int < 256 ->
  indexes_of_bits(Int, [], 0).

indexes_of_bits(_, Acc, 8) ->
  Acc;
indexes_of_bits(Int, Acc, Counter) when (Int bsr Counter) band 1 == 1 ->
  indexes_of_bits(Int, [7 - Counter|Acc], Counter + 1);
indexes_of_bits(Int, Acc, Counter) ->
  indexes_of_bits(Int, Acc, Counter + 1).
