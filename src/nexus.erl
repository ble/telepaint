-module(nexus).
-behaviour(gen_server).

%%external api
-export([start_link/0, start_link/1, shutdown/0, makeRoom/3, getRoomRef/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%state manipulation
-record(state, {roomsByShortID, maxRooms}).  

init_state(MaxRooms) ->
  #state{roomsByShortID = dict:new(), maxRooms = MaxRooms}.

%%external API
start_link() ->
  start_link(1). %default limits

start_link(RoomCount) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(RoomCount), []).

shutdown() ->
  ok = gen_server:call(?MODULE, shutdown).

getRoomRef(ShortID) ->
  gen_server:call(?MODULE, {getRoomID, ShortID}).

makeRoom(RoomID, CreatorID, RoomName) ->
  gen_server:call(?MODULE, {makeRoom, RoomID, CreatorID, tpaint_util:sanitize(RoomName)}).

%%gen server callbacks
init(State) -> 
  {ok, State}.

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call(
  {getRoomID, ShortID},
  _From,
  State = #state{roomsByShortID = RoomsByShortID}) ->
  {reply, dict:find(ShortID, RoomsByShortID), State};


handle_call(
  {makeRoom, RoomID, CreatorID, RoomName},
  _,
  State = #state{roomsByShortID = RoomsByShortID,
              maxRooms = MaxRooms}) ->
  ShortID = shorten(RoomID),
  case dict:is_key(ShortID, RoomsByShortID) of
    true -> {reply, collision, State};
    false ->
      NRooms = dict:size(RoomsByShortID),
      if
        NRooms >= MaxRooms ->
          {reply, {error, full}, State};
        true ->
          {ok, CreatorPID} = userServer:start(CreatorID, RoomID),
          {ok, RoomPID} = roomServer:start(RoomID, RoomName, CreatorID, CreatorPID),
          RoomRef = state:initializeRoomRef(RoomID, RoomName, CreatorID, RoomPID),
          {reply, {ok, ShortID}, State#state{roomsByShortID = dict:store(ShortID, RoomRef, RoomsByShortID)}}
      end
  end;

handle_call(Term, _, State) ->
  io:format("Unexpected call ~p\n", [Term]),
  {reply, error, State}.

handle_cast(_Request, State) ->
  io:format("~p: meaningless async call", [?MODULE]),
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("~p: meaningless oob call", [?MODULE]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

code_change(_, State, _) ->
  {ok, State}.

shorten(String) ->
  string:sub_string(String, 1, 8).
