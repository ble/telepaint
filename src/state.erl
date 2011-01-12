-module(state).
-include("state.hrl").
-export([initializeRoomRef/4, initializeRoom/4, successorMap/1,
         fileNameAsString/1, directoriesForPicRef/2]).

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
    extra = [],
    startedTime = erlang:localtime(),
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

directoriesForPicRef(
  StoreRoot,
  #picRef{
    roomName = RoomName,
    roomID = RoomID,
    roomStartDate = StartDate,
    startedBy = Starter,
    startedID = SID,
    drawnBy = Artiste,
    drawnID = DID,
    n = N}) ->
  RoomDirectory = [StoreRoot, $/, RoomName, RoomID],
  GameDirectory = [RoomDirectory, $/, formatDate(StartDate)],
  SDirectory = [GameDirectory, $/, Starter, SID],
  ADirectory = [GameDirectory, $/, Artiste, DID],
  {fileNameAsString(SDirectory), fileNameAsString(ADirectory)}.

filesForPicRef(
  StoreRoot,
  PicRef = #picRef{
    n = N,
    drawnBy = DrawnBy,
    startedBy = StartedBy},
  Extension) ->
  {StackDirectory, ArtistDirectory} = state:directoriesForPicRef(StoreRoot, PicRef), 
  NStr = io_lib:fwrite("~3..0B", N),
  SPath = [StackDirectory, $/, NStr, DrawnBy, Extension],
  APath = [ArtistDirectory, $/, NStr, StartedBy, Extension],
  {SPath, APath}.

fileNameAsString(Name) ->
  case lists:all(fun erlang:is_integer/1, Name) of
    true ->
      Name;
    false when is_atom(Name) ->
      erlang:atom_to_list(Name);
    false when is_list(Name) ->
      fileNameAsString([], Name);
    false when is_binary(Name) ->
      {error, "don't know how to handle the raw filename case."}
  end.

fileNameAsString(Accum, []) ->
  lists:flatten(lists:reverse(Accum));

fileNameAsString(Accum, [Char | Rest]) when is_integer(Char) ->
  {AlsoInString, Remainder} = lists:splitwith(fun erlang:is_integer/1, Rest),
  Prefix = [Char | AlsoInString],
  fileNameAsString([Prefix | Accum], Remainder);

fileNameAsString(Accum, [Atom | Rest]) when is_atom(Atom) ->
  fileNameAsString([erlang:atom_to_list(Atom) | Accum], Rest); 

fileNameAsString(Accum, [DeepList | Rest]) when is_list(DeepList) ->
  Deeper = fileNameAsString([], DeepList),
  fileNameAsString([Deeper | Accum], Rest).

formatDate({{Y, M, D}, {H, Min, S}}) ->
  lists:flatten(io_lib:format("~4..0B.~2..0B.~2..0B_~2..0B.~2..0B.~2..0B", [Y, M, D, H, Min, S])).
