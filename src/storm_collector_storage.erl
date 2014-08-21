-module(storm_collector_storage).

-behaviour(gen_server).

-export([start_link/0, add/1, all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

add(Item) ->
  gen_server:cast(?MODULE, {add, Item}).

all() ->
  gen_server:call(?MODULE, all).

%% callbacks
handle_cast({add, Item}, Storage) ->
  {noreply, add_item_with_timestamp(Item, Storage)}.

handle_call(all, _From, Storage) ->
  {reply, Storage, Storage}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% utility

add_item_with_timestamp(Item, Storage) ->
  Now = calendar:universal_time(),
  ItemWithTimestamp = maps:put(timestamp, Now, Item),
  [ ItemWithTimestamp | Storage ].
