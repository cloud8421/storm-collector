-module(storm_collector_storage).

-behaviour(gen_server).

-export([start_link/0, add/1, all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {vent, items=[]}).

-define(DATA_FILE, "data/weather_datapoints.dets").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, WeatherTable} = dets:open_file(weather_data, [{file, ?DATA_FILE}, {type, set}]),

  {ok, VentPid} = gen_event:start_link(),
  gen_event:add_handler(VentPid, log_handler, []),
  State = #state{vent=VentPid, items=WeatherTable},
  {ok, State}.

add(BinaryData) ->
  gen_server:cast(?MODULE, {add, BinaryData}).

all() ->
  gen_server:call(?MODULE, all).

%% callbacks
handle_cast({add, BinaryData}, State=#state{vent=VentPid, items=WeatherTable}) ->
  Item = process_binary(BinaryData),
  ItemWithMetadata = add_metadata_to_item(Item),
  Uuid = maps:get(uuid, ItemWithMetadata),
  dets:insert(WeatherTable, {Uuid, ItemWithMetadata}),
  gen_event:notify(VentPid, {datapoint, ItemWithMetadata}),
  {noreply, State}.

handle_call(all, _From, State = #state{vent=_VentPid, items=WeatherTable}) ->
  Items = dets:foldl(fun({_Uuid, Item}, Acc) ->
   [Item | Acc]
  end, [], WeatherTable),
  {reply, Items, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% utility

add_metadata_to_item(Item) ->
  Uuid = uuid:to_string(uuid:uuid1()),
  Now = calendar:universal_time(),
  Item#{uuid => Uuid, timestamp => Now}.

process_binary(BinaryData) ->
  Parsable = binary_to_list(BinaryData),
  Item = parser:parse(Parsable),
  External = forecast_client:fetch(),
  maps:merge(Item, External).
