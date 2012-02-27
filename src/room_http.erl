-module(room_http).

-include("room_context.hrl").
-include("room.hrl").
-export([req_context/1, authorized/1]).

get_bin_cookie(Key, Req) ->
  case wrq:get_cookie_value(Key, Req) of
    undefined -> undefined;
    X -> list_to_binary(X)
  end.

get_bin_path(Key, Req) ->
  case wrq:path_info(Key, Req) of
    undefined -> undefined;
    X -> list_to_binary(X)
  end.

%if the cookie and the url room ids agree,
%look up the room's pid on nexus and store all these
%with the observer id in the context.  
req_context(Req) ->
  UriRoomId = wrq:path_info(room_id, Req),
  CookieRoomId = get_bin_cookie("roomId", Req),

  UriObserverId = get_bin_path(observer_id, Req),
  CookieObserverId = get_bin_cookie("observerId", Req),
  ObserverId = case UriObserverId of
    undefined -> CookieObserverId;
    CookieObserverId -> CookieObserverId;
    _ -> undefined
  end,
  RoomId = list_to_binary(UriRoomId),
  IsCookied = (RoomId == CookieRoomId) and (ObserverId =/= undefined),
  RoomPid = case RoomId of
    undefined -> undefined;
    _ ->
      case nexus:lookup_room(nexus, RoomId) of
        {ok, Pid} -> Pid;
        {error, none_such} -> undefined
      end
  end,
  ObserverPid = if
    not IsCookied orelse (RoomPid =:= undefined) ->
      undefined;
    true ->
      case room:get_observer(RoomPid, ObserverId) of
        {error, Reason} ->
          undefined;
        {ok, Observer = #player{id = ObserverId}} ->
          Observer#player.pid
      end
  end,
  #room_context{
    room_id = RoomId,
    observer_id = ObserverId,
    room_pid = RoomPid,
    observer_pid = ObserverPid,
    is_cookied = IsCookied}.

%determine if the room exists and it has the observer
authorized(Ctx = #room_context{room_pid = RoomPid, observer_id = ObserverId}) ->
  case RoomPid of
    undefined -> false;
    _ ->
      {ok, Present} = room:has_observer(RoomPid, ObserverId),
      Present
  end.
