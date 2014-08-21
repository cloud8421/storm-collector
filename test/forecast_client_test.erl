-module(forecast_client_test).

-c(forecast_client).

-include_lib("eunit/include/eunit.hrl").

parses_response_test() ->
  Response = [{<<"latitude">>,51.519958},
               {<<"longitude">>,-0.099055},
               {<<"timezone">>,<<"Europe/London">>},
               {<<"offset">>,1},
               {<<"currently">>,
                [{<<"time">>,1408639649},
                 {<<"summary">>,<<"Partly Cloudy">>},
                 {<<"icon">>,<<"partly-cloudy-day">>},
                 {<<"nearestStormDistance">>,2},
                 {<<"nearestStormBearing">>,276},
                 {<<"precipIntensity">>,0},
                 {<<"precipProbability">>,0},
                 {<<"temperature">>,60.9},
                 {<<"apparentTemperature">>,60.9},
                 {<<"dewPoint">>,45.75},
                 {<<"humidity">>,0.57},
                 {<<"windSpeed">>,5.36},
                 {<<"windBearing">>,261},
                 {<<"visibility">>,9.32},
                 {<<"cloudCover">>,0.56},
                 {<<"pressure">>,1013.02},
                 {<<"ozone">>,338.16}]}],
  Expected = #{external_temperature => 60.9, summary => <<"Partly Cloudy">>},
  ?assert(forecast_client:parse_response(Response) == Expected).
