-module(gameState).

-include("state.hrl").
-export([initializeGameState/1, passStack/3, getTopStack/2]).
-record(gameState, {roomName, startDate, idOrder, playerByID, successorID, stacksHeldByID, completeStacks}).

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
    stacksHeldByID = dict:from_list(lists:zip(UserOrder, Stacks)),
    completeStacks = []}.

getTopStack(#gameState{stacksHeldByID = AllStacks}, PlayerID) ->
  case [Stacks || {ok, Stacks} <- [dict:find(PlayerID, AllStacks)]] of
    [] ->
      {error, "bad player id"};
    [[]] ->
      {ok, "noStack"};
    [[Stack | _Rest]] ->
      [Top | _More] = Stack,
      {ok, {struct, [{topSheet, Top#sheet.file}]}}
  end.

passStack(
  GameState = #gameState{
    playerByID = Players,
    stacksHeldByID = Stacks0,
    idOrder = IDs,
    successorID = NextID,
    completeStacks = CompleteStacks},
  PictureData,
  PassingPlayerID) ->
  N = length(IDs),
  case [Stacks || {ok, Stacks} <- [dict:find(PassingPlayerID, Stacks0)]] of
    [] ->
      {error, "bad player id"};

    [[]] ->
      {error, "player has no stacks"};

    [[Stack | _RemainingStacks]] when length(Stack) > N ->
      {error, "stack is already done"};

    [[Stack | RemainingStacks]] ->
      {ok, {_, _, PassingPlayerPID}} = dict:find(PassingPlayerID, Players),
      Stacks1 = dict:store(PassingPlayerID, RemainingStacks, Stacks0),  %remove stack from passer's pile

      PassedStack = addPictureToStack(PictureData, Stack, PassingPlayerID, GameState), %record the new picture in the stack
      CompleteStacks1 = case length(PassedStack)-1 of
        N ->
          %handle side effect messages
          PIDs = [PID || ID <- IDs, {ok, {_, _, PID}} <- [dict:find(ID, Players)]],
          Files = [File || Sheet <- PassedStack, File <- [Sheet#sheet.file]],
          Message = {struct, [{method, completedStack},{urls, {array, Files}}]},
          [userServer:enqueue(PID, Message) || PID <- PIDs],
          userServer:enqueue(PassingPlayerPID, {struct, [{method, gameDone}]}),
          %result value
          [PassedStack | CompleteStacks];
        _ ->
          CompleteStacks
      end,

      {ok, ReceivingPlayerID} = NextID(PassingPlayerID),
      {ok, {_, _, ReceivingPlayerPID}} = dict:find(ReceivingPlayerID, Players),
      Stacks2 = dict:append(ReceivingPlayerID, PassedStack, Stacks1), %add the stack to the receiver's pile
      {ok, ReceiversStacks} = dict:find(ReceivingPlayerID, Stacks2), %grab the receivers pile
      NewGameState = GameState#gameState{stacksHeldByID = Stacks2, completeStacks = CompleteStacks1},

      %let the passer know the pass completed; if there are other stacks waiting for the passer,
      %make the top stack ready for the passer to work on.
      userServer:enqueue(PassingPlayerPID, {struct, [{response, passStack}, {status, ok}]}),
      io:format(
        "Passer's stack count: ~b~nReceiver's stack count: ~b~n",
        [length(RemainingStacks), length(ReceiversStacks)]),
      if
        length(RemainingStacks) > 0 andalso length(hd(RemainingStacks)) =< N ->
          sendStackReady(PassingPlayerPID, RemainingStacks);
        true -> []
      end,
      if
        length(ReceiversStacks) == 1 andalso length(hd(ReceiversStacks)) =< N ->
          sendStackReady(ReceivingPlayerPID, ReceiversStacks);
        true -> []
      end,
      NewGameState
  end.

sendStackReady(PID, [[_TopSheet | [FilledSheet | _]] | _]) ->
  userServer:enqueue(PID, {struct, [{method, stackReady}, {imgUrl, FilledSheet#sheet.file}]}).
 
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
  UpdatedSheet = TopSheet#sheet{file = lists:flatten(Path)},
  {ok, ReceiverID} = SID(PassingPlayerID),
  {ok, {ReceiverID, ReceiverName, _}} = dict:find(ReceiverID, Players),
  NewSheet = #sheet{file = none, strokes = [], picRef = picRefWithDrawer(PicRef, ReceiverID, ReceiverName)},
  NewStack = [NewSheet | [UpdatedSheet | Sheets]],
  NewStack.

