-record(room,
  {id, name, game, observers}).

-record(game,
  {id, start_stamp, players, state}).

-record(game_state,
  {player_stacks, done_stacks}).

-record(player,
  {id, name, pid}).

-record(stack,
  {id, drawings}).

-record(drawing,
  {id, start_stamp, player_id, data}).

