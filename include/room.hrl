-record(room,
  {id, name, game, observers}).

-record(game,
  {id, start_stamp, players, stacks_by_player}).

-record(player,
  {id, name, pid}).

-record(stack,
  {id, drawings}).

-record(drawing,
  {id, start_stamp, player_id, data}).

