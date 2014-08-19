-module(parser).

-export([parse/1]).

parse(Data) ->
  Pairs = string:tokens(Data, "|"),
  toPairs(Pairs, []).

toPairs([], Acc) ->
  ReversedList = lists:reverse(Acc),
  maps:from_list(ReversedList);
toPairs([H|T], Acc) ->
  toPairs(T, [toPair(H) | Acc]).

toPair(Pair) ->
  Tokens = string:tokens(Pair, ":"),
  tokenize(Tokens).

tokenize(["temp", "f", Value]) ->
  {Temp, []} = string:to_float(Value),
  {temp, Temp};
tokenize(["bright", "i", Value]) ->
  {Brightness, []} = string:to_integer(Value),
  {bright, Brightness}.
