-record(roomState,
  {
    roomID,
    roomName,
    roomStartDate,
    inGame,
    gameTheme,
    users,
    userOrder,
    creatorID,
    gameState,
    extra
  }).

-record(roomRef,
  {
    roomID,
    roomName,
    roomPID,
    creatorID
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
    picRef,
    strokes,
    file
  }).

-record(picRef,
  {
    roomName,
    roomID,
    startDate,
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
