-module(json_view).
-include("room.hrl").

-export([
    room/3,
    observer/2]).

observer(Observer, SelfId) ->
  Items0 = [{<<"id">>, Observer#player.id}],
  Items1 = case Observer#player.id of
    SelfId ->
      [{<<"self">>, true} | Items0];
    _ -> Items0
  end,
  Items2 = case Observer#player.name of
    undefined ->
      Items1;
    Name ->
      [{<<"name">>, Name} | Items1]
  end,
  {Items2}.

room(Room, {Mega, Unit, Micro}, ObserverId) ->
  Observers = [observer(O, ObserverId) || O <- room_state:get_observers(Room)],
  {[
    {<<"type">>, <<"room">>},
    {<<"name">>, room_state:get_name(Room)},
    {<<"observers">>, Observers},
    {<<"when">>, [Mega, Unit, Micro]}
    ]}.
