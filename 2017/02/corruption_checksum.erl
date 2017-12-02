-module(corruption_checksum).
-export([file_checksum/1, checksum/1, file_div_checksum/1, div_checksum/1]).

% Part 1
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

% Part 2
file_div_checksum(Filename) ->
  div_checksum(read_spreadsheet(Filename)).

div_checksum(Spreadsheet) ->
  div_checksum(Spreadsheet, 0).

div_checksum([], Acc) ->
  Acc;
div_checksum([H | T], Acc) ->
  div_checksum(T, Acc + div_row_checksum(H)).

div_row_checksum([]) ->
  0;
div_row_checksum(Row) ->
  div_row_checksum(Row, Row).

div_row_checksum([Current | Rest], FullRow) ->
  case first_divisible(Current, FullRow) of
    none -> div_row_checksum(Rest, FullRow);
    [A, B] -> A div B
  end.

first_divisible(_, []) ->
  none;
first_divisible(X, [X | T]) ->
  first_divisible(X, T);
first_divisible(X, [H | _]) when X rem H =:= 0->
  [X, H];
first_divisible(X, [_ | T]) ->
  first_divisible(X, T).

% Util

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
