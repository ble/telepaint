-module(room_client_resource).
-export([init/1]).
-export([
    service_available/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_html/2,
    resource_exists/2
  ]).

-record(room_context, {room_id, observer_id, room_pid}).

-define(FORM_PATH, "priv/www/room_client.html").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #room_context{}}.

service_available(Req, Ctx) ->
  {true, Req, populate_context(Req, Ctx)}.


populate_context(Req, Ctx) ->
  UriRoomId = wrq:path_info(room_id, Req),
  CookieRoomId = wrq:get_cookie_value("roomId", Req),
  CookieObserverId = wrq:get_cookie_value("observerId", Req),
  RoomId = if
    UriRoomId == CookieRoomId ->
      list_to_binary(UriRoomId);
    true ->
      undefined
  end,
  RoomPidLookup = case RoomId of
    undefined ->
      undefined;
    _ ->
      nexus:lookup_room(nexus, RoomId)
  end,
  RoomPid = case RoomPidLookup of
    {ok, Pid} -> Pid;
    _ -> undefined
  end,
  Ctx#room_context{
    room_id = RoomId,
    observer_id = CookieObserverId,
    room_pid = RoomPid}.

is_authorized(Req, Ctx) ->
  Unauthorized = case Ctx#room_context.room_pid of
    undefined -> false;
    RoomPid ->
      {ok, Present} = room:has_observer(RoomPid, Ctx#room_context.observer_id),
      Present
  end,
  {not Unauthorized, Req, Ctx}.

allowed_methods(Req, Ctx) ->
  {['GET'], Req, Ctx}.

resource_exists(Req, Ctx) when
Ctx#room_context.room_pid =/= undefined ->
  {true, Req, Ctx};

resource_exists(Req, Ctx) ->
  {false, Req, Ctx}.

content_types_provided(Req, Ctx) ->
  {[{"text/html", to_html}], Req, Ctx}.

to_html(Req, Ctx) -> 
  {ok, Value} = file:read_file(?FORM_PATH),
  {Value, Req, Ctx}.



