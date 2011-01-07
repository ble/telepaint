-module(roomServer).
-include_lib("kernel/include/file.hrl").
-include("state.hrl").
-behaviour(gen_server).


-export([start/4, addNewUser/2, getStateSeenBy/2, getUserRef/2, chatMessage/3, nameWasSet/3]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(RoomID, RoomName, CreatorID, CreatorPID) ->
  gen_server:start(?MODULE, {RoomID, RoomName, CreatorID, CreatorPID}, []).

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
      {reply, ok, State#roomState{users=dict:store(UserID, UserRef, Users), userOrder=[UserID | Order]}}
  end;

handle_call({getUserRef, UserID}, _, State = #roomState{users = Users}) ->
  case dict:find(UserID, Users) of
    error ->
      {reply, error, State};
    {ok, UserRef} ->
      {reply, {ok, UserRef}, State}
  end;

handle_call({chatMessage, _UserID, Data}, _, State = #roomState{users = UserDict}) ->
  {_UserIDs, UserRefs} = lists:unzip(dict:to_list(UserDict)),
  UserPIDs = [ UserRef#userRef.pid || UserRef <- UserRefs ],
  lists:foreach(
    fun (UserPID) ->
      userServer:enqueue(UserPID, Data)
    end,
    UserPIDs),
  {reply, ok, State};

handle_call({nameWasSet, UserID, Name}, _, _State) ->
  %for now, do nothing
  %should probably notify all users that the name was set
  %or notify them that they should update their state
  {reply, ok, _State};

handle_call({getStateSeenBy, UserID}, _, State = #roomState{users = Users, userOrder = Order}) ->
  UserRefs = [UserRef || ID <- lists:reverse(Order), {ok, UserRef} <- [dict:find(ID, Users)]],
  UserNames = [{userServer:getName(PID), UserID == ID} || #userRef{pid=PID, sessionID=ID} <- UserRefs],
  UsersJSON = [case Name of
      undefined ->
        {struct, [{whoIs, WhoIs}]};
      _ ->
        {struct, [{whoIs, WhoIs}, {name, Name}]}
    end || {Name, IsYou} <- UserNames, WhoIs <- [case IsYou of true -> "you"; _ -> "other" end]],

  JSON = {struct, [{users, {array, UsersJSON}}, {name, State#roomState.roomName}, {inGame, State#roomState.inGame}]},
  {reply, JSON, State};

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
