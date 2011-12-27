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
      {ok, {[game_began | Events], Game1}}
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
      {ok, {[{player_joined, Player}], Game1}}
  end.

pass_stack(Game0, Player) ->
  State0 = Game0#game.state,
  Stacks0 = State0#game_state.player_stacks,
  Players = Game0#game.players,
  Count = length(Players),
  Index = index_of(Players, Player),
  PasserStacks0 = lists:reverse(lists:nth(Index, Stacks0)),
  if
    PasserStacks0 =:= [] ->
      {error, no_stack};
    true ->
      [PassedStack0 | RemainingStacks] = PasserStacks0,
      PasserStacks1 = lists:reverse(RemainingStacks),
      Stacks1 = replace_at(Stacks0, Index, PasserStacks1),
      Drawings = PassedStack0#stack.drawings,
      [DoneDrawing | _] = Drawings,
      Events0 = [
        {drawing_done, DoneDrawing#drawing.id},
        {passed_stack, {Player, PassedStack0}}],
      StackDone = length(Drawings) =:= Count,
      if
        StackDone ->
          State1 = State0#game_state{player_stacks=Stacks1},
          Game1 = Game0#game{state=State1},
          Events1 = Events0 ++ [{stack_done, PassedStack0}],
          case lists:flatten(Stacks1) of
            [] ->
              Events2 = Events1 ++ [game_done],
              Game2 = Game0#game{state=done},
              {ok, {Events2, Game2}};
            _ ->
              {ok, {Events1, Game1}}
          end;
        true ->
          NextIndex = 1 + (Index rem Count),
          Receiver = lists:nth(NextIndex, Players),
          ReceiverStacks0 = lists:nth(NextIndex, Stacks0), 
          {ok, {PassEvents, PassedStack1}} = passed_to(PassedStack0, Receiver),
          ReceiverStacks1 = [PassedStack1 | ReceiverStacks0],
          Stacks2 = replace_at(Stacks1, NextIndex, ReceiverStacks1),
          State2 = State0#game_state{player_stacks=Stacks2},
          Game3 = Game0#game{state=State2},
          Events3 = Events0 ++ PassEvents,
          {ok, {Events3, Game3}}
       end
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

stacks_and_players(Game) ->
  lists:zip(Game#game.players, (Game#game.state)#game_state.player_stacks).

test() ->
  P0 = #player{id = <<"id0">>},
  P1 = #player{id = <<"id1">>},
  {ok, G0} = game_state:make(),
  {ok, {J0, G1}} = game_state:add_player(G0, P0),
  {ok, {J1, G2}} = game_state:add_player(G1, P1),
  {ok, {E0, G3}} = game_state:start_game(G2),
  {ok, {E1, G4}} = game_state:pass_stack(G3, P0),
  {ok, {E2, G5}} = game_state:pass_stack(G4, P1),
  {ok, {E3, G6}} = game_state:pass_stack(G5, P1),
  {ok, {E4, G7}} = game_state:pass_stack(G6, P0),
  [J0, J1, E0, E1, E2, E3, E4].
