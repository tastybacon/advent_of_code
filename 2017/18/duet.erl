-module(duet).
-compile(export_all).

read_instructions(Filename) ->
	{ok, Data} = file:read_file(Filename),
  Instructions = binary:split(Data, [<<"\n">>], [global, trim_all]),
  parse_instructions(lists:map(fun binary_to_list/1, Instructions)).

parse_instructions(Instructions) ->
  lists:map(fun parse_instruction/1, Instructions).

parse_instruction(InstructionString) ->
  MatchData = re:run(
             InstructionString,
             "\\A(\\w{3}) (\\w)(?: (.*))?\\Z",
             [{capture, all_but_first, list}]
            ),
  case MatchData of
    {match, [Instruction, Target, Value]} ->
      {
       list_to_atom(Instruction),
       parse_value(Target),
       parse_value(Value)
      };
    {match, [Instruction, Target]} -> 
      {list_to_atom(Instruction), parse_value(Target)};
    nomatch -> none
  end.

parse_value(Value) ->
  Match = re:run(
             Value,
             "\\A[a-z]\\Z",
             [{capture, none}]
            ),
  case Match of
    match -> list_to_atom(Value);
    nomatch -> list_to_integer(Value)
  end.

create_registers() ->
  dict:from_list(
    lists:map(
      fun(X) -> { list_to_atom([X]), 0 } end,
      lists:seq($a, $z)
     )
   ).
  
run(Instructions, Registers, Position, State) when Position >= length(Instructions); Position < 0 ->
  {_, _, Parent, _} = State,
  Parent ! {done, Registers, State};
run(Instructions, Registers, Position, State) ->
  {Sent, Received, Parent, Sibling} = State,
  Instruction = lists:nth(Position + 1, Instructions),
  case Instruction of
    {snd, Target} ->
      Sibling ! {msg, get_value(Target, Registers)},
      run(Instructions, Registers, Position + 1, {Sent + 1, Received, Parent, Sibling});
    {set, Target, Value} ->
      NewValue = get_value(Value, Registers),
      run(Instructions, dict:store(Target, NewValue, Registers), Position + 1, State);
    {add, Target, Value} ->
      NewValue = get_value(Target, Registers) + get_value(Value, Registers),
      run(Instructions, dict:store(Target, NewValue, Registers), Position + 1, State);
    {mul, Target, Value} ->
      NewValue = get_value(Target, Registers) * get_value(Value, Registers),
      run(Instructions, dict:store(Target, NewValue, Registers), Position + 1, State);
    {mod, Target, Value} ->
      NewVal = get_value(Target, Registers) rem get_value(Value, Registers),
      run(Instructions, dict:store(Target, NewVal, Registers), Position + 1, State);
    {rcv, Target} ->
      receive
        {Pid, ping} ->
          Pid ! {waiting, State},
          run(Instructions, Registers, Position, State);
        {Pid, get} ->
          Pid ! {ok, Registers, Position, State},
          run(Instructions, Registers, Position, State);
        {msg, X} -> run(Instructions, dict:store(Target, X, Registers), Position + 1, {Sent, Received + 1, Parent, Sibling})
      end;
    {jgz, Target, Value} ->
      NewValue = get_value(Value, Registers),
      case get_value(Target, Registers) of
        X when X > 0 -> run(Instructions, Registers, Position + NewValue, State);
        _ -> run(Instructions, Registers, Position + 1, State)
      end
  end.

get_value(A, _) when is_integer(A) ->
  A;
get_value(A, Dict) ->
  dict:fetch(A, Dict).

create_program(Instructions) ->
  spawn(duet, init, [Instructions]).

create_program(Instructions, Registers) ->
  spawn(duet, init, [Instructions, Registers]).

create_pair(Instructions) ->
  {create_program(Instructions), create_program(Instructions)}.

create_pair(Instructions, RegistersA, RegistersB) ->
  {
   create_program(Instructions, RegistersA),
   create_program(Instructions, RegistersB)
  }.

start_pair(A, B, P) ->
  A ! {start, P, B},
  B ! {start, P, A},
  {A, B}.

ping(Pid) ->
  Pid ! {self(), ping},
  receive
    {waiting, State} -> State
  after
    1000 -> no_response
  end.

init(Instructions) ->
  init(Instructions, create_registers()).

init(Instructions, Registers) ->
  receive
    {start, Parent, Sibling} ->
      run(Instructions, Registers, 0, {0, 0, Parent, Sibling}),
      Parent ! ok
  end.
