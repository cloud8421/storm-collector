-module(api).
-compile({parse_transform, leptus_pt}).

%% leptus callbacks
-export([init/3]).
-export([get/3]).
-export([terminate/4]).

init(_Route, _Req, State) ->
    {ok, State}.

get("/datapoints", _Req, State) ->
  Datapoints = storm_collector_storage:all(),
  Serialized = storage_serializer:serialize(Datapoints),
  Json = jsx:encode(Serialized),
  {ok, Json, State}.

terminate(_Reason, _Route, _Req, _State) ->
  ok.
