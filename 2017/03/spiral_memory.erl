-module(spiral_memory).
-export([manhattan_distance/1, find_point/1, shell/1, test/0]).

origin() ->
  {point, 0, 0}.

add_point({point, X1, Y1}, {point, X2, Y2}) ->
  {point, X1 + X2, Y1 + Y2}.

% Defaults to distance from origin
manhattan_distance({point, _, _} = Point) ->
  manhattan_distance(Point, origin()).
manhattan_distance({point, X1, Y1}, {point, X2, Y2}) ->
  abs(X1 - X2) + abs(Y1 - Y2).

shell(X) ->
  shell(X, 1, 0).

shell(X, Base, Acc) ->
  case X =< math:pow(Base, 2) of
    true -> Acc;
    false -> shell(X, Base + 2, Acc + 1)
  end.

find_point(X) when X > 0 ->
  find_point(X, 1, origin(), origin()).

find_point(X, X, {point, _, _} = Point, _) ->
  Point;
find_point(X, CurrentIndex, {point, _, _} = Point, {point, _, _} = State) ->
  {NextPoint, NextState} = next_point(Point, State),
  find_point(X, CurrentIndex + 1, NextPoint, NextState).

% Initial position
next_point({point, 0, 0}, _) ->
  {{point, 1, 0}, {point, 0, 1}};
% Upper Right Corner
next_point({point, X, X}, _) when X > 0 ->
  {{point, X - 1, X}, {point, -1, 0}};
% Lower Left Corner
next_point({point, X, X}, _) ->
  {{point, X + 1, X}, {point, 1, 0}};
% Lower Right Corner
next_point({point, X, Y}, _) when X == abs(Y) ->
  {{point, X + 1, Y}, {point, 0, 1}};
% Upper Left Corner
next_point({point, X, Y}, _) when Y == abs(X) ->
  {{point, X, Y - 1}, {point, 0, -1}};
next_point({point, _, _} = P, {point, _, _} = DP) ->
  {add_point(P, DP), DP}.

  
  
test() ->
  test_find_point(1, {point, 0, 0}),
  test_find_point(2, {point, 1, 0}),
  test_find_point(3, {point, 1, 1}),
  test_find_point(4, {point, 0, 1}),
  test_find_point(5, {point, -1, 1}),
  test_find_point(6, {point, -1, 0}),
  test_find_point(7, {point, -1, -1}),
  test_find_point(8, {point, 0, -1}),
  test_find_point(9, {point, 1, -1}),
  test_find_point(10, {point, 2, -1}),
  test_find_point(12, {point, 2, 1}),
  test_find_point(23, {point, 0, -2}).

test_find_point(Input, Expected) ->
  Result = find_point(Input),
  io:format("Input: ~w | Expected: ~w | Got: ~w | Result ~w~n", [Input, Expected, Result, Result =:= Expected]).
