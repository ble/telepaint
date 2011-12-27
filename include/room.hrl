
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


%so if we ignore persistence entirely, what's the right model?
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

%Now that might not be the right model, but that is revealing.
%With persistence I'm leaning towards ids and a relational thang.
%With game state, I'm leaning towards containment relationships.
