-module(parser).

-export([parse/1]).

parse(Data) ->
  Pairs = string:tokens(Data, "|"),
  toPairs(Pairs, []).

toPairs([], Acc) -> lists:reverse(Acc);
toPairs([H|T], Acc) ->
  toPairs(T, [toPair(H) | Acc]).

toPair(Pair) ->
  Tokens = string:tokens(Pair, ":"),
  tokenize(Tokens).

tokenize(["temperature", "f", Value]) ->
  {Temp, []} = string:to_float(Value),
  {temperature, Temp};
tokenize(["brightness", "i", Value]) ->
  {Brightness, []} = string:to_integer(Value),
  {brightness, Brightness}.
