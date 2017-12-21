-module(tubes).
-compile(export_all).

read_map(Filename) ->
	{ok, Data} = file:read_file(Filename),
  Rows = binary:split(Data, [<<"\n">>], [global, trim_all]),
  parse_map(lists:map(fun binary_to_list/1, Rows)).

parse_map(Rows) ->
  parse_map(Rows, dict:new(), 0).

parse_map([], Acc, _) ->
  Acc;
parse_map([Row|T], Acc, RowIndex) ->
  parse_map(T, parse_row(Row, Acc, RowIndex), RowIndex + 1).

parse_row(Row, Acc, Index) ->
  {_, Result } = lists:foldl(
                   fun($ , State) ->
                       {X, Dict} = State,
                       {X + 1, Dict};
                      (Elem, State) ->
                       {X, Dict} = State,
                       {X + 1, dict:store({X, Index}, Elem, Dict)}
                   end,
                   {0, Acc},
                   Row
                  ),
  Result.

find_entrance(Map) ->
  Filtered = dict:to_list(
               dict:filter(fun({_, 0}, $|) -> true;
                              (_, _) -> false
                           end,
                           Map)
              ),
  [{{_, _} = Coord, _}] = Filtered,
  Coord.

get_path(Map) ->
  Entrance = find_entrance(Map),
  move(Entrance, {0,1}, Map, [], 1).

move(Position, Heading, Map, Acc, StepCounter) ->
  Straight = add_point(Position, Heading),
  case dict:find(Straight, Map) of
    {ok, Value} -> move(Straight, Heading, Map, track_value(Value, Acc), StepCounter + 1);
    _ -> 
      LeftHeading = head_left(Heading),
      Left = add_point(Position, LeftHeading),
      case dict:find(Left, Map) of
        {ok, Value} -> move(Left, LeftHeading, Map, track_value(Value, Acc), StepCounter + 1);
        _ ->
          RightHeading = head_right(Heading),
          Right = add_point(Position, RightHeading),
          case dict:find(Right, Map) of
            {ok, Value} ->
              move(Right, RightHeading, Map, track_value(Value, Acc), StepCounter + 1);
            _ -> {ok, dict:fetch(Position, Map), StepCounter, lists:reverse(Acc)}
          end
      end
  end.

track_value(Value, Acc) ->
  case Value of
    $| -> Acc;
    $- -> Acc;
    $+ -> Acc;
    X -> [X|Acc]
  end.

add_point({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.

head_left(Heading) ->
  case Heading of
    {0, 1} -> {1, 0};
    {0, -1} -> {-1, 0};
    {1, 0} -> {0, -1};
    {-1, 0} -> {0, 1}
  end.

head_right(Heading) ->
  case Heading of
    {0, 1} -> {-1, 0};
    {0, -1} -> {1, 0};
    {1, 0} -> {0, 1};
    {-1, 0} -> {0, -1}
  end.
