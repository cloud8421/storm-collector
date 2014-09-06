-module(parser).

-export([parse/1]).

parse(Data) ->
  Normalized = normalize(Data),
  Pairs = string:tokens(Normalized, "|"),
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

normalize(Data) ->
  case lists:reverse(Data) of
    "\n\r" ++ ReversedData -> lists:reverse(ReversedData);
    "\r\n" ++ ReversedData -> lists:reverse(ReversedData);
    _Else -> Data
  end.

