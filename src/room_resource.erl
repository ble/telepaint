-module(room_resource).
-export([init/1]).
-export([
    service_available/2,
    allowed_methods/2,
    resource_exists/2, 
    forbidden/2,

    content_types_provided/2, 
    to_json/2,

    post_is_create/2,
    process_post/2
  ]).

-include("room_context.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #room_context{}}.

service_available(Req, _) ->
  {true, Req, room_http:req_context(Req)}.

%allow if the room exists and it has the observer
forbidden(Req, Ctx) ->
  Authorized = case Ctx#room_context.room_pid of
    undefined -> false;
    RoomPid ->
      {ok, Present} = room:has_observer(RoomPid, Ctx#room_context.observer_id),
      Present
  end,
  {not Authorized, Req, Ctx}.

allowed_methods(Req, Ctx) ->
  {['GET', 'POST'], Req, Ctx}.

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

post_is_create(Req, Ctx) ->
  {false, Req, Ctx}.

process_post(Req, Ctx) ->
  case wrq:get_req_header("Content-Type", Req) of
    "application/json" ->
      process_json(Req, Ctx);
    _ ->
      {false, Req, Ctx}
  end.

process_json(Req, Ctx) -> 
  try
    Json = jiffy:decode(wrq:req_body(Req)),
    {PropList} = Json,
    Name = proplists:get_value(<<"name">>, PropList),
    room:name_observer(
      Ctx#room_context.room_pid,
      Ctx#room_context.observer_id,
      Name),
    RespJson = {[{<<"status">>, <<"ok">>}]},
    {true, wrq:set_resp_body(jiffy:encode(RespJson), Req), Ctx}
  catch
    {error, {_, invalid_json}} ->
      {false, Req, Ctx};
    X ->
      io:format("Unexpected error: ~p~n", [X]),
      {false, Req, Ctx}
  end.

to_json(Req, Ctx) ->
  case wrq:method(Req) of
    'GET' ->
      {ok, When, RoomState} = room:get_state(Ctx#room_context.room_pid),
      Json = json_view:room(RoomState, When, Ctx#room_context.observer_id),
      {jiffy:encode(Json), Req, Ctx};
    'POST' ->
      {jiffy:encode({[]}), Req, Ctx}
  end.


