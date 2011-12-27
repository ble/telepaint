
%There's some tension between appropriate forms for persisting
%metadata vs. storing active game state...
-record(room_state,
  {id, start_stamp, name, game_state, users}).

%all of these reference-y setups are probably better for persistence
%than active manipulation
-record(drawing,
  {id, start_stamp, end_stamp, stack_id, player_id, pred_id, succ_id}).

-record(stack,
  {id, tstamp, game_id, drawing_ids}).

-record(game,
  {id, start_stamp, end_stamp, room_id, stack_ids, player_ids}).
