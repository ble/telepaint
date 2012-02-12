-module(room_http).

-include("room_context.hrl").
-export([req_context/1]).

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
  CookieObserverId = get_bin_cookie("observerId", Req),
  RoomId = list_to_binary(UriRoomId),
  IsCookied = (RoomId == CookieRoomId) and (CookieObserverId =/= undefined),
  RoomPid = case RoomId of
    undefined -> undefined;
    _ ->
      {ok, Pid} = nexus:lookup_room(nexus, RoomId),
      Pid
  end,
  #room_context{
    room_id = RoomId,
    observer_id = CookieObserverId,
    room_pid = RoomPid,
    is_cookied = IsCookied}.

