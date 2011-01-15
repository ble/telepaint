-module(gameState).

-include("state.hrl").
-export([initializeGameState/1, passStack/3]).
-record(gameState, {roomName, startDate, idOrder, playerByID, successorID, stacksHeldByID}).

initializeGameState(
  #roomState{
    roomName = RoomName,
    roomID = RoomID,
    userOrder = UserOrder,
    users = Users}
) ->
  PlayerPIDs = [PID || ID <- UserOrder, {ok, #userRef{pid=PID}} <- [dict:find(ID, Users)]],
  PlayerNames = [userServer:getName(PID) || PID <- PlayerPIDs],
  PlayerTuples = lists:zip3(UserOrder, PlayerNames, PlayerPIDs),
  PlayersByID = dict:from_list(lists:zip(UserOrder, PlayerTuples)),
  SuccessorID = state:successorMap(UserOrder),
  StartTime = erlang:localtime(),
  PicRefs = [#picRef{
      roomName = RoomName,
      roomID = RoomID,
      startDate = StartTime,
      startedBy = Name,
      startedID = ID,
      drawnBy = Name,
      drawnID = ID,
      n = 0} || {ID, Name, _PID} <- PlayerTuples],
  Stacks = [[[#sheet{picRef = PicRef, strokes = [], file = none}]] || PicRef <- PicRefs],

  #gameState{
    roomName = RoomName,
    startDate = StartTime,
    idOrder = UserOrder,
    playerByID = PlayersByID,
    successorID = SuccessorID,
    stacksHeldByID = dict:from_list(lists:zip(UserOrder, Stacks))}.


passStack(GameState = #gameState{playerByID = Players, stacksHeldByID = Stacks0, idOrder = IDs, successorID = NextID}, PictureData, PassingPlayerID) ->
  N = length(IDs),
  case [Stacks || {ok, Stacks} <- [dict:find(PassingPlayerID, Stacks0)]] of
    [] ->
      {error, "bad player id"};

    [[]] ->
      {error, "player has no stacks"};

    [[Stack | _RemainingStacks]] when length(Stack) >= N ->
      {error, "stack is already done"};

    [[Stack | RemainingStacks]] ->
      {ok, {_, _, PassingPlayerPID}} = dict:find(PassingPlayerID, Players),
      Stacks1 = dict:store(PassingPlayerID, RemainingStacks, Stacks0),  %remove stack from passer's pile
      PassedStack = addPictureToStack(PictureData, Stack, PassingPlayerID, GameState), %record the new picture in the stack
      {ok, {ReceivingPlayerID, ReceivingPlayerPID}} = NextID(PassingPlayerID),
      Stacks2 = dict:append(ReceivingPlayerID, PassedStack, Stacks1), %add the stack to the receiver's pile
      {ok, ReceiversStacks} = dict:find(ReceivingPlayerID),
      NewGameState = GameState#gameState{stacksHeldByID = Stacks2},

      %let the passer know the pass completed; if there are other stacks waiting for the passer,
      %make the top stack ready for the passer to work on.
      userServer:enqueue(PassingPlayerID, {struct, [{response, passStack}, {status, ok}]}),
      if
        length(RemainingStacks) > 0 ->
          sendStackReady(PassingPlayerPID, RemainingStacks);
        true -> []
      end,
      if
        length(ReceiversStacks) == 1 ->
          sendStackReady(ReceivingPlayerPID, ReceiversStacks);
        true -> []
      end,
      NewGameState
  end.

sendStackReady(PID, [[TopSheet | _] | _]) ->
  userServer:enqueue(PID, {struct, [{response, stackReady}, {imgUrl, TopSheet#sheet.file}]}).
 
picRefWithRoom(PicRef, #roomState{roomName = RN, roomID = RI, roomStartDate = RSD}) ->
  PicRef#picRef{
    roomName = RN,
    roomID = RI,
    startDate = RSD,
    n = 0}.

picRefWithDrawer(PicRef, ID, Name) ->
  PicRef#picRef{
    drawnBy = Name,
    drawnID = ID,
    n = PicRef#picRef.n + 1}.

picRefWithStarter(PicRef, ID, Name) ->
  PicRef#picRef{
    startedBy = Name,  
    startedID = ID,
    drawnBy = Name,
    drawnID = ID,
    n = 0
  }.

addPictureToStack(
  PictureData,
  [TopSheet | Sheets],
  PassingPlayerID,
  #gameState{
    successorID = SID,
    playerByID = Players
  }) ->
  PicRef = TopSheet#sheet.picRef,
  {ok, Path} = diskStore:savePicture(PictureData, PicRef),
  UpdatedSheet = TopSheet#sheet{file = Path},
  {ok, ReceiverID} = SID(PassingPlayerID),
  {ok, {ReceiverID, ReceiverName, _}} = dict:find(ReceiverID, Players),
  NewSheet = #sheet{file = none, strokes = [], picRef = picRefWithDrawer(PicRef, ReceiverID, ReceiverName)},
  NewStack = [NewSheet | [UpdatedSheet | Sheets]],
  NewStack.

