-module(packet_scanners).
-compile(export_all).

% TODO: Figure out smart way to do part 2

read_input(Filename) ->
  {ok, Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global, trim]),
  lists:map(
    fun parse_line/1,
    Lines
   ).

parse_line(Line) ->
  [{Depth, _}, {Range, _}] = lists:map(
    fun string:to_integer/1,
    string:tokens(binary:bin_to_list(Line), ": ")
   ),
  {Depth, Range}.

severity(Trip) ->
  severity(Trip, 0, 0, 0).

severity(Trip, {offset, X}) ->
  severity(Trip, 0, X, 0).

severity(Time, Depth, Range) ->
  case scanner_position(Time, Range) of
    0 -> {Depth * Range, 1};
    _ -> {0, 0}
  end.

severity([], Acc, _, TimesCaught) ->
  {Acc, TimesCaught};
severity([{Depth, Range}|T], Acc, TimeOffset, TimesCaught) ->
  {Severity, Caught} = severity(TimeOffset + Depth, Depth, Range),
  severity(T, Severity + Acc, TimeOffset, TimesCaught + Caught).

scanner_position(Time, Range) when Range > 0 ->
  MaximumIndex = Range - 1,
  TwoWayIndex = 2 * Range - 2,
  DoubleIndex = case TwoWayIndex of
                  X when X < 2 -> 0;
                  X -> Time rem X
                end,
  case DoubleIndex > MaximumIndex of
    true -> MaximumIndex - DoubleIndex rem MaximumIndex;
    false -> DoubleIndex
  end.

find(Fun, List) ->
  find(Fun, List, 0).

find(_, [], _) ->
  false;
find(Fun, [H|T], Index) ->
  case Fun(H) of
    true -> {H, Index};
    _ -> find(Fun, T, Index + 1)
  end.
