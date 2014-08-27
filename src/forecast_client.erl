-module(forecast_client).

-export([fetch/0, parse_response/1]).

-define(LAT, 51.5199579).
-define(LNG, -0.0990549).

fetch() ->
  {ok, _StatusCode, _RespHeaders, Client} = hackney:get(api_url(),
                                                         [], <<>>, []),
  {ok, Body} = hackney:body(Client),
  Decoded = jsx:decode(Body),
  parse_response(Decoded).

api_url() ->
  Lat = float_to_list(?LAT, [{decimals, 6}]),
  Lng = float_to_list(?LNG, [{decimals, 6}]),
  "https://api.forecast.io/forecast/" ++ api_key() ++ "/" ++ Lat ++ "," ++ Lng ++ "?exclude=minutely,hourly,daily,alerts,flags&units=si".

parse_response(Body) ->
  {<<"currently">>, Currently} = lists:keyfind(<<"currently">>, 1, Body),
  {<<"temperature">>, Temperature} = lists:keyfind(<<"temperature">>, 1, Currently),
  {<<"summary">>, Summary} = lists:keyfind(<<"summary">>, 1, Currently),
  {<<"pressure">>, Pressure} = lists:keyfind(<<"pressure">>, 1, Currently),
  #{external_temperature => Temperature, summary => Summary, pressure => Pressure}.

api_key() ->
  os:getenv("FORECASTIO_API_KEY").
