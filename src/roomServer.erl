-module(roomServer).
-include_lib("kernel/include/file.hrl").
-include("state.hrl").
-behaviour(gen_server).


-export([start/4, shutdown/1, addNewUser/2, getStateSeenBy/2, getUserRef/2,
         chatMessage/3, nameWasSet/3, stroke/3, beginGame/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(RoomID, RoomName, CreatorID, CreatorPID) ->
  gen_server:start(?MODULE, {RoomID, RoomName, CreatorID, CreatorPID}, []).

shutdown(PID) ->
  gen_server:call(PID, shutdown).

getStateSeenBy(PID, UserID) ->
  gen_server:call(PID, {getStateSeenBy, UserID}).

getUserRef(PID, UserID) ->
  gen_server:call(PID, {getUserRef, UserID}).

addNewUser(PID, UserID) ->
  gen_server:call(PID, {addNewUser, UserID}).

chatMessage(PID, UserID, Data) ->
  gen_server:call(PID, {chatMessage, UserID, Data}).

nameWasSet(PID, UserID, Name) ->
  gen_server:call(PID, {nameWasSet, UserID, Name}).

stroke(PID, UserID, Params) ->
  gen_server:call(PID, {stroke, UserID, Params}).

beginGame(PID) ->
  gen_server:call(PID, beginGame).

init({RoomID, RoomName, CreatorID, CreatorPID}) -> 
  {ok, state:initializeRoom(RoomID, RoomName, CreatorID, CreatorPID)}.

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call({addNewUser, UserID}, _, State = #roomState{roomID = RoomID, users=Users, userOrder=Order}) ->
  case dict:is_key(UserID, Users) of
    true ->
      {reply, {error, "hash collision"}, State};
    false ->
      {ok, UserPID} = userServer:start(UserID, RoomID),
      UserRef = #userRef{sessionID = UserID, roomID = RoomID, pid = UserPID},
      {reply, ok, State#roomState{users=dict:store(UserID, UserRef, Users), userOrder= Order ++ [UserID]}}
  end;

handle_call({getUserRef, UserID}, _, State = #roomState{users = Users}) ->
  case dict:find(UserID, Users) of
    error ->
      {reply, error, State};
    {ok, UserRef} ->
      {reply, {ok, UserRef}, State}
  end;

handle_call({chatMessage, UserID, Message}, _, State = #roomState{userOrder = Order, users = Users}) ->
  case dict:find(UserID, Users) of
    error ->
      {reply, error, State};
    {ok, #userRef{pid = PID}} ->
      Name = userServer:getName(PID),
      case itemIndex(Order, UserID) of
        error ->
          {reply, error, State};
        Index ->
        MessageFun =
          fun(_, ID) ->
            WhoIs = case ID of
              UserID -> "you";
              _ -> "other"
            end,
          {struct, [{method, chat}, {message, Message}, {from, Name}, {fromIndex, Index}, {whoIs, WhoIs}]}
        end,
        sendFnToUsers( MessageFun, State ),
        {reply, ok, State}
      end
    end;

handle_call({nameWasSet, UserID, Name}, _, State = #roomState{userOrder = Order}) ->
  case itemIndex(Order, UserID) of
    error ->
      {reply, {error, "bad user id"}, State};
    Index ->
      sendToUsers(
        {struct, [{method, nameWasSet}, {userIndex, Index}, {name, Name}]},
        State),
      {reply, ok, State}
  end;

handle_call({stroke, UserID, Params}, _, State = #roomState{users = Users, inGame = false, extra = Stuff}) ->
  case dict:is_key(UserID, Users) of
    false ->
      {reply, {error, "no such user in room"}, State};
    true ->
      StrokeFields = canonicalStroke(Params),
      sendFnToUsers( fun(_, ID) ->
            WhoIs = case ID of
              UserID -> "you";
              _ -> "other"
            end,
            {struct, [{whoIs, WhoIs} | StrokeFields]}
        end,
        State),
      Extra = [{struct, StrokeFields} | Stuff],
      {reply, ok, State#roomState{extra=Extra}}
  end;


handle_call(
  {getStateSeenBy, UserID},
  _,
  State = #roomState{
    users = Users, 
    creatorID = CreatorID,
    userOrder = Order, 
    inGame = InGame, 
    roomName = RoomName}) ->
  UserRefs = [UserRef || ID <- Order, {ok, UserRef} <- [dict:find(ID, Users)]],
  UserNames = [{userServer:getName(PID), UserID == ID, ID} || #userRef{pid=PID, sessionID=ID} <- UserRefs],
  UsersJSON = [case Name of
      undefined ->
        {struct, [{whoIs, WhoIs}, {isCreator, IsCreator}]};
      _ ->
        {struct, [{whoIs, WhoIs}, {name, Name}, {isCreator, IsCreator}]}
    end || {Name, IsYou, ID} <- UserNames,
           WhoIs <- [case IsYou of true -> "you"; _ -> "other" end],
           IsCreator <- [ID == CreatorID]],
  Fields =
    [ {users, {array, UsersJSON}},
      {name, RoomName},
      {inGame, InGame},
      {preGame, {array, case InGame of false -> State#roomState.extra; _ -> [] end}}],
  {reply, {struct, Fields}, State};

handle_call(beginGame, _, State = #roomState{inGame = false}) ->
  sendToUsers({struct, [{method, beginGame}]}, State),
  {reply, ok, State#roomState{inGame = true}};

handle_call(beginGame, _, State = #roomState{inGame = true}) ->
  {reply, alreadyBegan, State#roomState{inGame = true}};

handle_call(Request, _, State) ->
  {reply, {unknownRequest, Request}, State}.

handle_cast(_, State) ->
  io:format("~p: meaningless async call", [?MODULE]),
  {noreply, State}.

handle_info(_, State) ->
  io:format("~p: meaningless oob call", [?MODULE]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

code_change(_, State, _) ->
  {ok, State}.

itemIndex(List, Item) ->
  itemIndex(List, Item, 1).

itemIndex([Item | _Rest], Item, N) ->
  N;

itemIndex([_ | Rest], Item, N) ->
  itemIndex(Rest, Item, N+1);

itemIndex([], _, _) ->
  error.

mapUsers( Fn, #roomState{userOrder = Order, users = Users} ) ->
  Indices = lists:seq(1, length(Order)),
  Refs = [Ref || ID <- Order, {ok, Ref} <- [dict:find(ID, Users)]],
  IDs =  [ID || #userRef{sessionID = ID} <- Refs],
  PIDs = [PID || #userRef{pid = PID} <- Refs],
  lists:map(
    fun({Index, UserID, PID}) ->
        Fn(Index, UserID, PID)
    end,
    lists:zip3(Indices, IDs, PIDs)).

sendToUsers( Message, State ) ->
  Mapper = fun (_, _, PID) -> userServer:enqueue(PID, Message) end,
  mapUsers(Mapper, State).

sendFnToUsers( MessageFn, State ) ->
  Mapper =
    fun (Index, ID, PID) ->
      userServer:enqueue(PID, MessageFn(Index, ID))
    end,
  mapUsers(Mapper, State).

canonicalStroke(Params) ->
  case dict:find("coordinates", Params) of
    error ->
      error;
    {ok, Coordinates} ->
      [{method, stroke}, {coordinates, Coordinates}]
  end.
