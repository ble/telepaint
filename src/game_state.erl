-module(game_state).
-include("room.hrl").

-export([make/0, add_player/2, start_game/1, pass_stack/2]).
-export([test/0]).
-compile(export_all).
make() ->
  {ok, #game{id=id_unique:for(game), start_stamp=erlang:now(), players=[], state=waiting}}.

start_game(Game0) ->
  Started = has_started(Game0),
  if
    Started ->
      {error, already_started};
    true ->
      Players = Game0#game.players,
      {AllEvents, Stacks} = lists:unzip(lists:map(
        fun(Player) ->
            Stack = #stack{id=id_unique:for(stack), drawings=[]},
            {ok, {Es, NewStack}} = passed_to(Stack, Player),
            {Es, [NewStack]}
        end, Players)), 
      GameState = #game_state{player_stacks = Stacks, done_stacks = []},
      Game1 = Game0#game{state = GameState},
      Events = lists:flatten(AllEvents),
      {ok, {Events, Game1}}
  end.

passed_to(Stack0, Player) ->
  Drawing = #drawing{
    id=id_unique:for(drawing),
    start_stamp = erlang:now(),
    player_id = Player#player.id,
    data = none},
  Stack1 = Stack0#stack{drawings = [Drawing | Stack0#stack.drawings]},
  Events = [{new_drawing, Drawing#drawing.id}, {got_stack, {Player, Stack1}}],
  {ok, {Events, Stack1}}.

has_started(Game) ->
  Game#game.state =/= waiting.

has_player(Game, Id) ->
  [P || P <- Game#game.players, P#player.id =:= Id] =/= [].

add_player(Game0, Player) ->
  Started = has_started(Game0),
  Present = has_player(Game0, Player#player.id),
  if
    Started ->
      {error, too_late};
    Present ->
      {error, already_present};
    true ->
      Players0 = Game0#game.players,
      Game1 = Game0#game{players = [Player | Players0]},
      {ok, Game1}
  end.

pass_stack(Game, Player) ->
  State0 = Game#game.state,
  Stacks0 = State0#game_state.player_stacks,
  Players = Game#game.players,
  Count = length(Players),
  Index = index_of(Players, Player),
  NextIndex = (Index + 1) rem Count,
  PasserStacks0 = lists:reverse(lists:nth(Index, Stacks0)),
  case PasserStacks0 of
    [First | Rest] ->
      Receiver = lists:nth(NextIndex, Players),
      ReceiverStacks0 = lists:nth(NextIndex, Stacks0), 
      {ok, {Events0, PassedStack}} = passed_to(First, Receiver),
      ReceiverStacks1 = [PassedStack | ReceiverStacks0],
      PasserStacks1 = Rest,
      Stacks1 = replace_at(Stacks0, Index, PasserStacks1),
      Stacks2 = replace_at(Stacks1, NextIndex, ReceiverStacks1),
      State1 = State0#game_state{player_stacks=Stacks2},
      Game1 = Game#game{state=State1},
      [DoneDrawing | _] = First#stack.drawings,
      Events1 = [
        {drawing_done, DoneDrawing#drawing.id},
        {passed_stack, {Player, PassedStack, Receiver}} | Events0],
      {ok, {Events1, Game1}};
    [] ->
      {error, no_stack}
  end.

replace_at(List, Index, Value) -> replace_at([], List, Index, Value).

replace_at(Before, [_Replaced | After], 1, Value) ->
  lists:reverse(Before, [Value | After]);

replace_at(Before, [H | T], N, V) ->
  replace_at([H | Before], T, N-1, V).

index_of([], _Item) -> {error, empty};
index_of(L, Item) -> index_of(1, L, Item).

index_of(_, [], _Item) -> {error, not_found};
index_of(N, [Item | _], Item) -> N;
index_of(N, [_ | Tail], Item) -> index_of(N + 1, Tail, Item).


test() ->
  {ok, G0} = game_state:make(),
  {ok, G1} = game_state:add_player(G0, {player,a,b,c}),
  {ok, G2} = game_state:add_player(G1, {player,d,e,f}),
  {ok, {E0, G3}} = game_state:start_game(G2),
  {ok, {E1, G4}} = game_state:pass_stack(G3, {player,a,b,c}),
  [E0, E1, G4].
