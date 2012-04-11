-module(game).

-include("room.hrl").
-export([call_game/3]).

call_game(Player, {Func, Args}, Game)
  when is_atom(element(1, Game)) ->
    Module = element(1, Game),
    GameState0 = element(2, Game),
    try
      {ok, GameState1, Msgs} = apply(Module, Func, Args ++ [Player, GameState0]),
      {ok, {Module, GameState1}, Msgs}
    catch
      error:ErrReason ->
        {error, ErrReason};
      throw:ErrReason ->
        {error, ErrReason};
      'EXIT':ErrReason ->
        {error, ErrReason}
    end;

call_game(_Player, _Action, _Game) ->
  {error, bad_game_action}.
