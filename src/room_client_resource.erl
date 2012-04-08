-module(room_client_resource).
-export([init/1]).
-export([
    service_available/2,
    allowed_methods/2,
    forbidden/2,
    content_types_provided/2,
    to_html/2,
    resource_exists/2
  ]).

-define(FORM_PATH, "priv/www/room_client.html").
-include_lib("webmachine/include/webmachine.hrl").
-include("room_context.hrl").

init([]) ->
  {ok, #room_context{}}.

%service availability is equated to the existence and
%aliveness of the desired room process.
%TODO: make a non-running process give a different error
service_available(Req, _) ->
  Ctx = room_http:req_context(Req),
  Available = case Ctx#room_context.room_pid of
    undefined -> false;
    Pid -> is_process_alive(Pid)
  end,
  {Available, Req, Ctx}.


forbidden(Req0, Ctx) ->
  RoomPid = Ctx#room_context.room_pid,
  RoomId = Ctx#room_context.room_id,
  AlreadyInRoom = case Ctx#room_context.is_cookied of
    true ->
      {ok, Present} = room:has_observer(RoomPid, Ctx#room_context.observer_id),
      Present;
    _ -> false
  end,
  case not AlreadyInRoom andalso room:allow_anonymous_join(RoomPid) of
    true ->
      {ok, ObserverId} = room:add_observer(RoomPid),
      Path = binary_to_list(list_to_binary([<<"/room/">>, RoomId, <<"/">>])),
      Cookie1 = mochiweb_cookies:cookie("roomId", RoomId, [{path, Path}]),
      Cookie2 = mochiweb_cookies:cookie("observerId", ObserverId, [{path, Path}]),
      Loc = {"Location", [<<"/room/">>, Ctx#room_context.room_id, <<"/client?join">>]},
      Req1 = wrq:merge_resp_headers([Loc, Cookie1, Cookie2], Req0),
      %redirect basically just to start running
      %the room client resource from the start.
      {{halt, 303}, Req1, Ctx};
    _ -> 
      {not AlreadyInRoom, Req0, Ctx}
  end.

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



