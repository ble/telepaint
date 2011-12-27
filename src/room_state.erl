-module(room_state).
-include("room.hrl").

-export([make/1, add_observer/1, name_observer/3]).

make(Name) ->
  {ok, #room{id = id_unique:for(room), name=Name, game=none, observers=[]}}.

add_observer(Room0) ->
  Id = id_unique:for(player),
  Observer = #player{id=Id, name=none, pid=none},
  Observers = [Observer | Room0#room.observers],
  Room1 = Room0#room{observers=Observers},
  {ok, {Room1, Id}}.

get_observer(Room, Id) ->
  Obs = Room#room.observers,
  case [O || O <- Obs, O#player.id =:= Id] of
    [] -> {error, nonesuch};
    [Target] -> {ok, Target};
    _ -> {error, duplicate}
  end.

put_observer(Room, Observer) ->
  Id = Observer#player.id,
  Obs0 = Room#room.observers,
  Obs1 = [Observer | [O || O <- Obs0, O#player.id =/= Id]],
  {ok, Room#room{observers = Obs1}}.

name_observer(Room0, Id, Name) ->
  case get_observer(Room0, Id) of
    {ok, O} ->
      Tag = case O#player.name of
        none -> set;
        _ -> rename
      end,
      {ok, Room1} = put_observer(Room0, O#player{name=Name}),
      {ok, {Tag, Room1}};
    X ->
      X
  end.

bind_observer(Room0, Id, Pid) ->
  case get_observer(Room0, Id) of
    {ok, O} ->
      put_observer(Room0, O#player{pid=Pid});
    X ->
      X
  end.

