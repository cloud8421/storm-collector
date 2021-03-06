-module(storm_collector_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    hackney:start(),
    leptus:start_listener(http, [{'_', [{api, undefined_state}]}]),
    storm_collector_sup:start_link().

stop(_State) ->
    ok.
