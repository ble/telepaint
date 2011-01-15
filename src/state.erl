-module(state).
-include("state.hrl").
-export([initializeRoomRef/4, initializeRoom/4, successorMap/1]).

initializeRoomRef(RoomID, RoomName, CreatorID, RoomPID) ->
  #roomRef{
    roomID = RoomID,
    roomName = RoomName,
    creatorID = CreatorID,
    roomPID = RoomPID
  }.

initializeRoom(RoomID, RoomName, CreatorID, CreatorPID) ->
  UserRef = #userRef{sessionID = CreatorID, roomID = roomID, pid = CreatorPID},
  #roomState{
    roomID = RoomID,
    roomName = RoomName,
    roomStartDate = erlang:localtime(),
    creatorID = CreatorID,
    inGame = false,
    gameTheme = none,
    users = dict:from_list([{CreatorID, UserRef}]),
    userOrder = [CreatorID],
    gameState = none,
    extra = []
  }.



% initializeGameState(
%   RoomState = #roomState{
%     users = UsersByID,
%     userOrder = UserOrder
%   }) ->
%   BasePicRef = picRefWithRoom(#picRef{}, RoomState),
%   PIDs = [PID
%     || ID <- UserOrder,
%        {ok, #userRef{pid = PID}} <- [dict:find(ID, UsersByID)]],
%   Names = [userServer:getName(PID)
%     || PID <- PIDs], 
%   PicRefs = [picRefWithStarter(BasePicRef, ID, Name)
%     || {ID, Name} <- lists:zip(UserOrder, Names)],
%   StackSets = [ [[#sheet{picRef = PicRef, strokes = [], file = none}]]
%     || PicRef <- PicRefs], 
%   #gameState{
%     nPlayers = length(UserOrder),
%     nextPlayer = successorMap(UserOrder),
%     sheetsDone = 0,
%     stacksByPlayer = dict:from_list(lists:zip(UserOrder, StackSets))}.

rotateLeft(N, List) ->
  {FirstN, Rest} = lists:split(N, List),
  Rest ++ FirstN.

dictAsFun(Dict) ->
  fun (Key) -> 
      dict:find(Key, Dict)
  end.

successorMap(List) ->
  dictAsFun(dict:from_list(lists:zip(List, rotateLeft(1, List)))).

%mapMap :: (A -> B) -> [A] -> Map A B
mapMap(F, List) ->
  dict:from_list(lists:zip(List, lists:map(F, List))).

makeStroke(Width, Color, Coordinates) ->
  #stroke{width=Width, color=Color, coordinates=Coordinates}.


