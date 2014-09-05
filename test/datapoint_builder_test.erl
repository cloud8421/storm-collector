-module(datapoint_builder_test).

-c(datapoint_builder).

-include_lib("eunit/include/eunit.hrl").

-include("../src/datapoint.hrl").

builds_datapoint_test() ->
  mock_forecast_client(),
  mock_uuid(),
  mock_calendar(),
  Parsed = [{temperature,23.4}, {brightness,4000}],
  Expected = #datapoint{uuid="83b2bbda-354a-11e4-a328-b8e8563a72e8",
                        temperature=23.4,
                        brightness=4000,
                        external_temperature=18.6,
                        external_pressure=3000,
                        summary= <<"Partly Cloudy">>,
                        timestamp={{2014,9,5},{22,23,18}}},
  ?assertEqual(datapoint_builder:build(Parsed), Expected).

mock_uuid() ->
  FakeUuid= <<131,178,187,218,53,74,17,228,163,40,184,232,86,58,114,232>>,
  meck:new(uuid, [passthrough]),
  meck:expect(uuid, uuid1, fun() -> FakeUuid end).

mock_forecast_client() ->
  meck:new(forecast_client),
  meck:expect(forecast_client, fetch, fun() ->
    [{external_temperature, 18.6},
     {external_pressure, 3000},
     {summary, <<"Partly Cloudy">>}]
  end).

mock_calendar() ->
  Datetime={{2014,9,5},{22,23,18}},
  meck:new(calendar, [unstick]),
  meck:expect(calendar, universal_time, fun() -> Datetime end).
