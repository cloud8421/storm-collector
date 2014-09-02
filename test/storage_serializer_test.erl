-module(storage_serializer_test).

-c(storage_serializer).

-include_lib("eunit/include/eunit.hrl").

serializes_to_list_test() ->
  Datapoints = [#{bright => 912,
     external_temperature => 18.99,
     summary => <<"Partly Cloudy">>,
     temp => 24.7,
     timestamp => {{2014,8,22},{17,10,48}},
     uuid => "42e419d6-2a1f-11e4-b6c6-b8e8563a72e8"},
   #{bright => 911,
     external_temperature => 18.99,
     summary => <<"Partly Cloudy">>,
     temp => 23.7,
     timestamp => {{2014,8,22},{17,10,47}},
     uuid => "422cde60-2a1f-11e4-9742-b8e8563a72e8"}],
  Expected = [
    [{<<"bright">>,912},
     {<<"external_temperature">>,18.99},
     {<<"summary">>,<<"Partly Cloudy">>},
     {<<"temp">>,24.7},
     {<<"timestamp">>,<<"2014:8:22-17:10:48">>},
     {<<"uuid">>,<<"42e419d6-2a1f-11e4-b6c6-b8e8563a72e8">>}],
    [{<<"bright">>,911},
     {<<"external_temperature">>,18.99},
     {<<"summary">>,<<"Partly Cloudy">>},
     {<<"temp">>,23.7},
     {<<"timestamp">>,<<"2014:8:22-17:10:47">>},
     {<<"uuid">>,<<"422cde60-2a1f-11e4-9742-b8e8563a72e8">>}]],
  ?assert(storage_serializer:serialize(Datapoints) == Expected).
