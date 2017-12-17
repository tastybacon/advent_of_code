-module(permutation_promenade).
-compile(export_all).

% TODO: Optimization to be done in exchange/partner moves
% as well as find_multiple

read_instructions(Filename) ->
	{ok, Data} = file:read_file(Filename),
  [InstructionString] = binary:split(Data, [<<"\n">>], [global, trim_all]),
  parse_instructions(string:tokens(binary_to_list(InstructionString), ",")).

dancers(Count) ->
  dancers(Count, []).

dancers(0, Acc) ->
  lists:reverse(Acc);
dancers(Count, []) ->
  dancers(Count - 1, [a]);
dancers(Count, [H|_] = Acc) ->
  [PreviousChar] = atom_to_list(H),
  dancers(Count - 1, [list_to_atom([PreviousChar + 1])|Acc]).

parse_instructions(Instructions) ->
  lists:map(fun parse_instruction/1, Instructions).

parse_instruction([$s|T]) ->
  {Int, _} = string:to_integer(T),
  {spin, Int};
parse_instruction([$x|T]) ->
  Positions = string:tokens(T, "/"),
  [A, B] = lists:map(
                fun(X) -> {Int, _} = string:to_integer(X), Int end,
                Positions),
  {exchange, {A, B}};
parse_instruction([$p|T]) ->
  Positions = string:tokens(T, "/"),
  [A, B] = lists:map(fun list_to_atom/1, Positions),
  {partner, {A, B}}.

dance(List, []) ->
  List;
dance(List, [Instruction|T]) ->
  dance(step(List, Instruction), T).

dance_multiple(Count, List, Instructions) ->
  CycleLength = find_cycle(List, Instructions, 1000),
  ActualCount = Count rem CycleLength,
  dance_multiple(ActualCount, List, Instructions, CycleLength).

dance_multiple(0, List, _, _) ->
  List;
dance_multiple(Count, List, Instructions, _) ->
  NewPositions = dance(List, Instructions),
  dance_multiple(Count - 1, NewPositions, Instructions).

find_cycle(List, Instructions, MaxAttempts) ->
  find_cycle(0, List, Instructions, List, MaxAttempts).
find_cycle(Attempts, List, _, List, _) when Attempts > 0 ->
  Attempts;
find_cycle(Attempts, WorkingList, Instructions, List, MaxAttempts) when Attempts =< MaxAttempts ->
  find_cycle(Attempts + 1, dance(WorkingList, Instructions), Instructions, List, MaxAttempts);
find_cycle(_, _, _, _, _) ->
  none.

step(List, {Instruction, Args}) ->
  Fun = case Instruction of
          spin -> fun spin/2;
          exchange -> fun exchange/2;
          partner -> fun partner/2
        end,
  Fun(List, Args).

% Dance Moves: Spin, Exchange, Partner

spin(List, Count) ->
  Index = length(List) - Count,
  A = lists:sublist(List, Index),
  B = lists:sublist(List, Index + 1, Count),
  B ++ A.

exchange(List, {A, B}) ->
  Array = case array:is_array(List) of
            true -> List;
            false -> array:from_list(List)
          end,
  ValueA = array:get(A, Array),
  ValueB = array:get(B, Array),
  Swapped = array:set(A, ValueB, array:set(B, ValueA, Array)),
  array:to_list(Swapped).

partner(List, {A, B}) ->
  [{_, IndexA}, {_, IndexB}] = find_multiple(List, [A, B]),
  exchange(List, {IndexA, IndexB}).

find_multiple(List, Values) ->
  find_multiple(List, Values, [], 0).

find_multiple([], _, Acc, _) ->
  lists:reverse(Acc);
find_multiple([H|T], Values, Acc, Index) ->
  case lists:member(H, Values) of
    true -> find_multiple(T, Values, [{H, Index}|Acc], Index + 1);
    false -> find_multiple(T, Values, Acc, Index + 1)
  end.
