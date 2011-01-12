-record(roomState,
  {
    roomID,
    roomName,
    inGame,
    gameTheme,
    users,
    userOrder,
    creatorID,
    gameState,
    extra,
    startedTime
  }).

-record(roomRef,
  {
    roomID,
    roomName,
    roomPID,
    creatorID
  }).

-record(gameState,
  {
    nPlayers,
    nextPlayer,
    stacksByPlayer
  }).


-record(user,
  {
    sessionID,
    roomID,
    name,
    pid,
    mailbox,
    pollingPIDs
  }).

-record(userRef,
  {
    sessionID,
    roomID,
    pid
  }).

-record(sheet,
  {
    belongsTo,
    roomID,
    image,
    strokes
  }).

-record(picRef,
  {
    roomName,
    roomID,
    roomStartDate,
    startedBy,
    startedID,
    drawnBy,
    drawnID,
    n
  }).

-record(stroke,
  {
    width,
    color,
    coordinates
  }).

-record(stateDiskStore,
  {
    storeRoot
  }).
