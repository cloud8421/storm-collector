-module(storage_serializer).

-export([serialize/1]).

serialize(Datapoints) ->
  AsList = lists:map(fun(X) -> maps:to_list(X) end, Datapoints),
  lists:map(fun(DataPoint) ->
    do_serialize(DataPoint, [])
  end, AsList).

do_serialize([], Acc) -> lists:reverse(Acc);
do_serialize([{timestamp, Timestamp}|T], Acc) ->
  NewItem = {<<"timestamp">>, list_to_binary(utils:timestamp_to_string(Timestamp))},
  do_serialize(T, [NewItem|Acc]);
do_serialize([{uuid, Uuid}|T], Acc) ->
  NewItem = {<<"uuid">>, list_to_binary(Uuid)},
  do_serialize(T, [NewItem|Acc]);
do_serialize([{Key, Value}|T], Acc) ->
  NewItem = {atom_to_binary(Key, utf8), Value},
  do_serialize(T, [NewItem|Acc]).
