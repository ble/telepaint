-module(room_http).

-include("room_context.hrl").
-export([req_context/1, authorized/1]).

get_bin_cookie(Key, Req) ->
  case wrq:get_cookie_value(Key, Req) of
    undefined -> undefined;
    X -> list_to_binary(X)
  end.

%if the cookie and the url room ids agree,
%look up the room's pid on nexus and store all these
%with the observer id in the context.  
req_context(Req) ->
  UriRoomId = wrq:path_info(room_id, Req),
  CookieRoomId = get_bin_cookie("roomId", Req),

  UriObserverId = wrq:path_info(observer_id, Req),
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
  #room_context{
    room_id = RoomId,
    observer_id = ObserverId,
    room_pid = RoomPid,
    is_cookied = IsCookied}.

%determine if the room exists and it has the observer
authorized(Ctx = #room_context{room_pid = RoomPid, observer_id = ObserverId}) ->
  case RoomPid of
    undefined -> false;
    _ ->
      {ok, Present} = room:has_observer(RoomPid, Ctx#room_context.observer_id),
      Present
  end.
