-module(datapoint_builder).

-export([build/1]).

-include("../src/datapoint.hrl").

build([{temperature, Temperature}, {brightness, Brightness}]) ->
  [{external_temperature, ExternalTemperature},
   {external_pressure, ExternalPressure},
   {summary, Summary}] = forecast_client:fetch(),
  #datapoint{uuid=uuid(),
             temperature=Temperature,
             brightness=Brightness,
             external_temperature=ExternalTemperature,
             external_pressure=ExternalPressure,
             summary=Summary,
             timestamp=calendar:universal_time()}.

uuid() ->
  uuid:to_string(uuid:uuid1()).
