-module(reverse_captcha).
-export([captcha/1, test/0, captcha_part_2/1, test_part_2/0]).

captcha([H | _] = Captcha) ->
  captcha(Captcha ++ [H], none, 0).

captcha([H | T], H, Acc) ->
  captcha(T, H, Acc + H);
captcha([H | T], _, Acc) ->
  captcha(T, H, Acc);
captcha([], _, Acc) ->
  Acc.

test() ->
  test1(),
  test2(),
  test3(),
  test4().

test_case(Input, Expected) ->
  io:format("Input: ~w | Expected: ~w | Result ~w~n", [Input, Expected, captcha(Input) =:= Expected]).

test1() ->
  test_case([1,1,2,2], 3).

test2() ->
  test_case([1,1,1,1], 4).

test3() ->
  test_case([1,2,3,4], 0).

test4() ->
  test_case([9,1,2,1,2,1,2,9], 9).


captcha_part_2(Captcha) ->
  captcha_part_2(Captcha, Captcha ++ Captcha, length(Captcha) div 2 + 1, 0).

captcha_part_2([H | T], [_ | Y] = List, Offset, Acc) ->
  case lists:nth(Offset, List) of
    H ->
      captcha_part_2(T, Y, Offset, Acc + H);
    _ ->
      captcha_part_2(T, Y, Offset, Acc)
  end;
captcha_part_2([], _, _, Acc) ->
  Acc.

test_part_2() ->
  test1_part_2(),
  test2_part_2(),
  test3_part_2(),
  test4_part_2(),
  test5_part_2().

test_part_2_case(Input, Expected) ->
  Result = captcha_part_2(Input),
  io:format("Input: ~w | Expected: ~w | Got: ~w | Result ~w~n", [Input, Expected, Result, Result =:= Expected]).

test1_part_2() ->
  test_part_2_case([1,2,1,2], 6).

test2_part_2() ->
  test_part_2_case([1,2,2,1], 0).

test3_part_2() ->
  test_part_2_case([1,2,3,4,2,5], 4).

test4_part_2() ->
  test_part_2_case([1,2,3,1,2,3], 12).

test5_part_2() ->
  test_part_2_case([1,2,1,3,1,4,1,5], 4).
