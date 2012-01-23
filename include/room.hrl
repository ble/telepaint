-type timestamp() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type id() :: binary().
-type event() :: term().

-record(player,
  {id = <<>> :: id(),
   name = undefined :: binary() | undefined,
   pid :: pid() | undefined}).

-record(drawing,
  {id :: id(),
   start_stamp :: timestamp(),
   player_id :: id(),
   data :: term()}).

-record(stack,
  {id :: id(),
   drawings :: list(#drawing{})}).

-record(game_state,
  {player_stacks :: list(list(#stack{})),
   done_stacks :: list(#stack{})}).

-record(game,
  {id :: id(),
   start_stamp :: timestamp(),
   players :: list(#player{}),
   state :: waiting | #game_state{} | done}).

-record(room,
  {id = <<>> :: id(),
   name = <<>> :: binary(),
   game :: #game{} | undefined,
   observers = [] :: list(#player{})}).


