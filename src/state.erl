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
    creatorID = CreatorID,
    inGame = false,
    gameTheme = none,
    users = dict:from_list([{CreatorID, UserRef}]),
    userOrder = [CreatorID],
    gameState = none,
    extra = []
  }.

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


initializeGameState(#roomState{ users = Players, inGame = false }) ->
  {ok,
   #gameState{
     nPlayers = length(Players),
     nextPlayer = successorMap(Players),
     stacks = mapMap(
       fun (Player) ->
         [initializeSheet(Player)] end,
       Players)}};

initializeGameState(#roomState{ inGame = true }) ->
  {error, "Game already started."}.

messageToAllUsers(#roomState{users = Users}, Message) ->
  lists:foreach(
    fun (PID) -> PID ! {message, Message} end,
    lists:map(
      fun (#user{pid = UPID}) -> UPID end,
      Users)).

messageToParticularUser(Target, #roomState{users = Users}, Targeted, General) ->
  lists:foreach(
    fun (User = #user{pid = PID}) ->
        if
          User == Target ->
            PID ! {message, Targeted};
          General /= noMessage ->
            PID ! {message, General};
          true -> nothing
        end
    end,
    Users).




initializeSheet(User = #user{roomID = RoomID}) ->
  #sheet{belongsTo = User, roomID = RoomID, image = none, strokes = []}.

makeStroke(Width, Color, Coordinates) ->
  #stroke{width=Width, color=Color, coordinates=Coordinates}.
