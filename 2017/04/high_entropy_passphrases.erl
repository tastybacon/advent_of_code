-module(high_entropy_passphrases).
-compile(export_all).

count_valid_in_file(Filename) ->
  count_valid(parse_passphrases_part1(read_passphrase_file(Filename))).

count_valid(Passphrases) ->
  count_valid(Passphrases, 0).

count_valid([], Acc) ->
  Acc;
count_valid([H | T], Acc) ->
  Count = case passphrase_valid(H) of
            true -> 1;
            false -> 0
          end,
  count_valid(T, Acc + Count).

passphrase_valid(Passphrase) when length(Passphrase) > 1 ->
  erlang:length(Passphrase) == sets:size(sets:from_list(Passphrase));
passphrase_valid(_) ->
  false.

read_passphrase_file(Filename) ->
	{ok, Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

parse_passphrases_part1(Passphrases) ->
  lists:map(fun parse_row_part1/1, Passphrases).

parse_row_part1(<<>>) ->
  [];
parse_row_part1(Row) ->
  binary:split(Row, [<<" ">>], [global]).

% Part 2

count_valid_in_file_part2(Filename) ->
  count_valid(parse_passphrases_part2(read_passphrase_file(Filename))).

parse_passphrases_part2(Passphrases) ->
  lists:map(fun parse_row_part2/1, Passphrases).

parse_row_part2(<<>>) ->
  [];
parse_row_part2(Row) ->
  lists:map(
    fun lists:sort/1,
    lists:map(
      fun binary_to_list/1,
      binary:split(Row, [<<" ">>], [global])
     )
   ).
