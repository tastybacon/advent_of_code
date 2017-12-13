-module(digital_plumber).
-export(
   [
    read_input/1,
    build_networks/1,
    programs_connected_to/2
   ]
  ).

read_input(Filename) ->
  {ok, Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global, trim]),
  lists:map(
    fun parse_line/1,
    Lines
   ).

parse_line(Line) ->
  MatchData = re:run(
             Line,
             "\\A(\\d+) <-> (.*)\\Z",
             [{capture, all_but_first, list}]
            ),
  case MatchData of
    {match, [ProgramIdString, ConnectedProgramsString]} ->
      {ProgramId, _} = string:to_integer(ProgramIdString),
      ConnectedPrograms = parse_connected_programs(ConnectedProgramsString),
      {ProgramId, ConnectedPrograms};
    nomatch -> none
  end.
  
parse_connected_programs(RawConnectedPrograms) ->
  parse_connected_programs(string:tokens(RawConnectedPrograms, ", "), []).

parse_connected_programs([], Acc) ->
  Acc;
parse_connected_programs([H|T], Acc) ->
  {ProgramId, _} = string:to_integer(H),
  parse_connected_programs(T, [ProgramId|Acc]).

build_networks(Programs) ->
  build_networks(Programs, []).

build_networks([], Acc) ->
  Acc;
build_networks([Program|Rest], Acc) ->
  {ProgramId, ConnectedProgramIds} = Program,
  CurrentNetwork = sets:from_list([ProgramId|ConnectedProgramIds]),
  {UnconnectedNetworks, ConnectedNetworks} = lists:partition(fun(X) -> sets:is_disjoint(X, CurrentNetwork) end, Acc),
  ConnectedNetwork = sets:union([CurrentNetwork|ConnectedNetworks]),
  io:format("Connected to ~w: ~w~n", [ProgramId, sets:to_list(ConnectedNetwork)]),
  build_networks(Rest, [ConnectedNetwork|UnconnectedNetworks]).

programs_connected_to(ProgramId, Networks) ->
  find(fun(X) -> sets:is_element(ProgramId, X) end, Networks).

find(Fun, List) ->
  find(Fun, List, 0).
find(_, [], _) ->
  false;
find(Fun, [H|T], Index) ->
  case Fun(H) of
    true -> {H, Index};
    _ -> find(Fun, T, Index + 1)
  end.
