-module(log_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

-include("datapoint.hrl").

init([]) ->
  {ok, []}.

handle_event({datapoint, Datapoint}, State) ->
  lager:info("~s\n", [datapoint_to_logline(Datapoint)]),
  {ok, State};

handle_event(_, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%% utility
datapoint_to_logline(Datapoint) ->
  Timestamp = Datapoint#datapoint.timestamp,
  Temp = Datapoint#datapoint.temperature,
  Brightness = Datapoint#datapoint.brightness,

  TempString = float_to_list(Temp, [{decimals, 1}]),
  BrightnessString = integer_to_list(Brightness),
  TimestampString = timestamp_to_string(Timestamp),
  "[timestamp: " ++ TimestampString ++ ", "
  "temp: " ++ TempString ++ ", "
  "brightness: " ++ BrightnessString ++ "]".

timestamp_to_string({{Year, Month, Day}, {Hour, Minutes, Seconds}}) ->
  Date = lists:map(fun(X) -> integer_to_list(X) end, [Year, Month, Day]),
  Time = lists:map(fun(X) -> integer_to_list(X) end, [Hour, Minutes, Seconds]),
  string:join(Date, ":") ++ "-" ++ string:join(Time, ":").
