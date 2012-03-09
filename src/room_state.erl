-module(room_state).
-include("room.hrl").
-include("rpc_methods.hrl").

-export([
    make/1,
    add_observer/1,
    name_observer/3,
    bind_observer/3,
    get_observers/1,
    get_observer/2,
    get_name/1]).

-spec make(Name :: binary()) -> {ok, #room{}}.
make(Name) ->
  {ok, #room{id = id_unique:for(room), name=Name, game=undefined, observers=[]}}.

-spec get_observers(Room :: #room{}) -> [#player{}].
get_observers(Room) -> Room#room.observers.

-spec add_observer(Room0 :: #room{}) -> {ok, {#room{}, id()}}.
add_observer(Room0) ->
  Id = id_unique:for(player),
  Observer = #player{id=Id},
  Observers = [Observer | Room0#room.observers],
  Room1 = Room0#room{observers=Observers},
  Messages = [json_rpc:as_call(#join_room{who = Id, name = undefined})],
  {ok, {Room1, Id, Messages}}.

-spec get_observer(Room :: #room{}, Id :: id()) ->
  {ok, #player{}} |
  {error, atom()}. 
get_observer(Room, Id) ->
  Obs = Room#room.observers,
  case [O || O <- Obs, O#player.id =:= Id] of
    [] -> {error, none_such};
    [Target] -> {ok, Target};
    _ -> {error, duplicate}
  end.

-spec put_observer(Room0 :: #room{}, Observer :: #player{}) -> {ok, #room{}}.
put_observer(Room, Observer) ->
  Id = Observer#player.id,
  Obs0 = Room#room.observers,
  Obs1 = [Observer | [O || O <- Obs0, O#player.id =/= Id]],
  {ok, Room#room{observers = Obs1}}.

-spec name_observer(Room0 :: #room{}, Id :: id(), Name :: binary()) ->
  {ok, {set | rename, #room{}}} |
  {error, atom()}.
name_observer(Room0, Id, Name) ->
  case get_observer(Room0, Id) of
    {ok, O} ->
      Tag = case O#player.name of
        undefined -> set;
        _ -> rename
      end,
      {ok, Room1} = put_observer(Room0, O#player{name=Name}),
      Messages = [json_rpc:as_call(#set_name{who = Id, name = Name})],
      {ok, {Room1, Tag, Messages}};
    X ->
      X
  end.

-spec bind_observer(Room0 :: #room{}, Id :: id(), Pid :: pid()) ->
  {ok, #room{}} |
  {error, atom()}.
bind_observer(Room0, Id, Pid) ->
  case get_observer(Room0, Id) of
    {ok, O} ->
      put_observer(Room0, O#player{pid=Pid});
    X ->
      X
  end.

-spec get_name(Room :: #room{}) -> binary().
get_name(Room) -> Room#room.name.
