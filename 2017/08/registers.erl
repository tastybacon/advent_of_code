-module(registers).
-compile(export_all).

% {instruction, TargetRegister, Operation, Value, Condition}
% {condition, Register, Operator, Value}

read_instructions(Filename) ->
  {ok, Data} = file:read_file(Filename),
  lists:map(
    fun parse_instruction/1,
    binary:split(Data, [<<"\n">>], [global, trim])
   ).

parse_instruction(Line) ->
  MatchData = re:run(
             Line,
             "\\A(\\w*) (\\w*) (-?\\d*) if (\\w*) ([><=!]=?) (-?\\d*)\\Z",
             [{capture, all_but_first, list}]
            ),
  case MatchData of
    {match, [TargetRegister, Operation, ValueString, CondRegister, ConditionOp, CondValueString]} ->
      {Value, _} = string:to_integer(ValueString),
      {CondValue, _} = string:to_integer(CondValueString),
      {instruction, TargetRegister, Operation, Value, {condition, CondRegister, ConditionOp, CondValue}};
    nomatch -> none
  end.

initialize_registers(InstructionList) ->
  initialize_registers(InstructionList, dict:new()).

initialize_registers([], Registers) ->
  Registers;
initialize_registers([H|T], Registers) ->
  {instruction, Register, _, _, _} = H,
  case dict:is_key(Register, Registers) of
    true -> initialize_registers(T, Registers);
    false -> initialize_registers(T, dict:store(Register, 0, Registers))
  end.

run(InstructionList) ->
  run(InstructionList, initialize_registers(InstructionList), 0).

run([], Registers, MaxValue) ->
  {Registers, MaxValue};
run([H|T], Registers, CurrentMaxValue) ->
  {NewRegisters, MaxValue} = step(H, Registers, CurrentMaxValue),
  run(T, NewRegisters, MaxValue).

step({instruction, _, _, _, {condition, _, _, _} = Condition} = Instruction, Registers, MaxValue) ->
  case test(Condition, Registers) of
    true -> execute(Instruction, Registers, MaxValue);
    false -> {Registers, MaxValue}
  end.

test({condition, Register, Operation, TargetValue}, Registers) ->
  CurrentValue = dict:fetch(Register, Registers),
  case Operation of
    "==" -> CurrentValue == TargetValue;
    "!=" -> CurrentValue /= TargetValue;
    ">=" -> CurrentValue >= TargetValue;
    "<=" -> CurrentValue =< TargetValue;
    ">" -> CurrentValue > TargetValue;
    "<" -> CurrentValue < TargetValue;
    _ -> badop
  end.

execute({instruction, TargetRegister, "inc", Value, _}, Registers, MaxValue) ->
  CurrentValue = dict:fetch(TargetRegister, Registers),
  NewValue = CurrentValue + Value,
  {dict:store(TargetRegister, NewValue, Registers), lists:max([NewValue, MaxValue])};
execute({instruction, TargetRegister, "dec", Value, _}, Registers, MaxValue) ->
  CurrentValue = dict:fetch(TargetRegister, Registers),
  NewValue = CurrentValue - Value,
  {dict:store(TargetRegister, NewValue, Registers), lists:max([NewValue, MaxValue])}.

largest_register(Registers) ->
  dict:fold(fun largest_register/3, none, Registers).

largest_register(Register, Value, none) ->
  largest_register(Register, Value, {Register, Value});
largest_register(Register, Value, {_, Acc}) when Value > Acc ->
  {Register, Value};
largest_register(_, _, Acc) ->
  Acc.
