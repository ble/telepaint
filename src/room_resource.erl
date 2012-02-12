-module(room_resource).
-export([init/1]).
-export([
    service_available/2,
    allowed_methods/2,
    forbidden/2,
    content_types_provided/2,
    to_json/2,
    resource_exists/2
  ]).

-include("room_context.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #room_context{}}.

service_available(Req, _) ->
  {true, Req, room_http:req_context(Req)}.

%allow if the room exists and it has the observer
forbidden(Req, Ctx) ->
  io:format("~p~n", [Ctx]),
  Authorized = case Ctx#room_context.room_pid of
    undefined -> false;
    RoomPid ->
      {ok, Present} = room:has_observer(RoomPid, Ctx#room_context.observer_id),
      Present
  end,
  {not Authorized, Req, Ctx}.

allowed_methods(Req, Ctx) ->
  {['GET'], Req, Ctx}.

%resource does not exist when the room cannot be looked up
resource_exists(Req, Ctx) when
Ctx#room_context.room_pid =/= undefined ->
  {true, Req, Ctx};

resource_exists(Req, Ctx) ->
  {false, Req, Ctx}.

content_types_provided(Req, Ctx) ->
  {
    [{"application/json", to_json}],
    Req,
    Ctx
  }.

to_json(Req, Ctx) ->
  {ok, When, RoomState} = room:get_state(Ctx#room_context.room_pid),
  Json = json_view:room(RoomState, When, Ctx#room_context.observer_id),
  {jiffy:encode(Json), Req, Ctx}.


