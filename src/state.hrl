-record(roomState,
  {
    roomID,
    roomName,
    inGame,
    gameTheme,
    users,
    userOrder,
    creatorID,
    gameState
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
    stacks
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

-record(stroke,
  {
    width,
    color,
    coordinates
  }).


