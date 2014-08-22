-module(storm_collector_storage).

-behaviour(gen_server).

-export([start_link/0, add/1, all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {vent, items=[]}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, VentPid} = gen_event:start_link(),
  gen_event:add_handler(VentPid, log_handler, []),
  State = #state{vent=VentPid, items=[]},
  {ok, State}.

add(BinaryData) ->
  gen_server:cast(?MODULE, {add, BinaryData}).

all() ->
  gen_server:call(?MODULE, all).

%% callbacks
handle_cast({add, BinaryData}, #state{vent=VentPid, items=Items}) ->
  Item = process_binary(BinaryData),
  ItemWithTimestamp = add_timestamp_to_item(Item),
  NewState = #state{vent=VentPid, items = [ItemWithTimestamp | Items]},
  gen_event:notify(VentPid, {datapoint, ItemWithTimestamp}),
  {noreply, NewState}.

handle_call(all, _From, State = #state{vent=_VentPid, items=Items}) ->
  {reply, Items, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% utility

add_timestamp_to_item(Item) ->
  Now = calendar:universal_time(),
  Item#{timestamp => Now}.

process_binary(BinaryData) ->
  Parsable = binary_to_list(BinaryData),
  Item = parser:parse(Parsable),
  External = forecast_client:fetch(),
  maps:merge(Item, External).
