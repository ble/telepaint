



-record(mural,
  {mural_hash, timestamp, mural_name, img_fetch_url, img_hash, rows, columns}).

-type user_pseudonym() :: string().
-type row() :: integer().
-type column() :: integer().
-type user_type() ::
  creator |
  { participant,
    user_pseudonym(),
    row(),
    column() } |
  observer.

-record(participant,
  {pseudonym, row, col}).

-record(user,
  {user_hash, bound_host, mural_hash, user_type, last_msg_time, resp_current}).

-record(img,
  {img_hash, img_binary}).

-record(chat,
  {u_timestamp, mural_hash, json_binary}).

-record(condense_log,
  {mural_hash, condense_timestamp, json_iolist}).

-record(stroke,
  {u_timestamp, mural_hash, user_hash, row, column, json_binary}).
