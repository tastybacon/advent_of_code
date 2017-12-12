-module(hex_ed).
-export(
   [
    read_steps/1,
    simplify_steps/1,
    steps_away/1
   ]
  ).

read_steps(Filename) ->
  {ok, Data} = file:read_file(Filename),
  [String] = binary:split(Data, [<<"\n">>], [global, trim]),
  lists:map(
    fun(X) -> list_to_atom(binary:bin_to_list(X)) end,
    binary:split(String, [<<",">>], [global, trim])
   ).

coord(Direction) ->
  case Direction of
    n -> {0, 2};
    ne -> {1, 1};
    se -> {1, -1};
    s -> {0, -2};
    sw -> {-1, -1};
    nw -> {-1, 1}
  end.

add_coord({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.

simplify_steps(Steps) ->
  simplify_steps(Steps, {0,0}, 0).

simplify_steps([], Coord, MaxDistance) ->
  {Coord, MaxDistance};
simplify_steps([Next|Rest], Coord, MaxDistance) ->
  NewCoord = add_coord(coord(Next), Coord),
  NewMax = lists:max([MaxDistance, steps_away(NewCoord)]),
  simplify_steps(Rest, NewCoord, NewMax).

steps_away({A, B}) ->
  (abs(A) + abs(B)) div 2;
steps_away(Steps) ->
  {{A, B}, MaxDistance} = simplify_steps(Steps),
  {steps_away({A, B}), MaxDistance}.
