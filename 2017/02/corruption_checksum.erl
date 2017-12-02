-module(corruption_checksum).
-compile(export_all).

checksum(Spreadsheet) ->
  checksum(Spreadsheet, 0).

checksum([H | T], Acc) ->
  checksum(T, Acc + row_checksum(H));
checksum([], Acc) ->
  Acc.

row_checksum([]) ->
  0;
row_checksum(Row) ->
  row_checksum(Row, none, none).

row_checksum([H | T], none, none) ->
  row_checksum(T, H, H);
row_checksum([H | T], Max, Min) when H > Max ->
  row_checksum(T, H, Min);
row_checksum([H | T], Max, Min) when H < Min ->
  row_checksum(T, Max, H);
row_checksum([_ | T], Max, Min) ->
  row_checksum(T, Max, Min);
row_checksum([], Max, Min) ->
  Max - Min.

file_checksum(Filename) ->
  checksum(read_spreadsheet(Filename)).

read_spreadsheet(Filename) ->
	{ok, Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  lists:map(fun parse_row/1, Lines).

parse_row(<<>>) ->
  [];
parse_row(Row) ->
  lists:map(fun parse_item/1, binary:split(Row, [<<"\t">>], [global])).

parse_item(Item) ->
  erlang:list_to_integer(erlang:binary_to_list(Item)).
