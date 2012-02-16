-module(json_view).
-include("room.hrl").

-export([
    room/3,
    observer/2]).

observer(Observer, SelfId) ->
  NameString = case Observer#player.name of
    undefined ->
      <<"{ anonymous observer }">>;
    Name ->
      [<<"< ">>, Name, <<" >">>]
  end,
  Items0 = [{<<"name">>, NameString}, {<<"id">>, Observer#player.id}],
  Items1 = case Observer#player.id of
    SelfId ->
      [{<<"self">>, true} | Items0];
    _ -> Items0
  end,
  {Items1}.

room(Room, {Mega, Unit, Micro}, ObserverId) ->
  Observers = [observer(O, ObserverId) || O <- room_state:get_observers(Room)],
  {[
    {<<"type">>, <<"room">>},
    {<<"name">>, <<"pit of despair">>},
    {<<"observers">>, Observers},
    {<<"when">>, [Mega, Unit, Micro]}
    ]}.
