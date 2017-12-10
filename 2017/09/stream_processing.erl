-module(stream_processing).
-compile(export_all).

read_stream(Filename) ->
  {ok, Data} = file:read_file(Filename),
  [Stream] = binary:split(Data, [<<"\n">>], [global, trim]),
  binary:bin_to_list(Stream).

parse(Stream) ->
  parse(Stream, 0, 0, 0).

parse([], Acc, 0, GarbageCount) ->
  {Acc, GarbageCount};
parse([H|T], Acc, Level, GarbageCount) ->
  case H of
    ${ -> parse(T, Acc, Level + 1, GarbageCount);
    $} -> parse(T, Acc + Level, Level - 1, GarbageCount);
    $< -> parse_garbage(T, Acc, Level, GarbageCount);
    _ -> parse(T, Acc, Level, GarbageCount)
  end.

parse_garbage([H|T], Acc, Level, GarbageCount) ->
  case H of
    $> -> parse(T, Acc, Level, GarbageCount);
    $! ->
      [_|Rest] = T,
      parse_garbage(Rest, Acc, Level, GarbageCount);
    _ -> parse_garbage(T, Acc, Level, GarbageCount + 1)
  end.
