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
-include("rpc.hrl").
-include("rpc_methods.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, #room_context{}}.

service_available(Req, _) ->
  {true, Req, room_http:req_context(Req)}.

%allow if the room exists and it has the observer
forbidden(Req, Ctx) ->
  {not room_http:authorized(Ctx), Req, Ctx)}.

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
    Body = wrq:req_body(Req),
    Json = jiffy:decode(Body),
    Call = json_rpc:unjif(Json),

    Params = Call#rpc_call.params,
    Method = element(1, Params),

    case Method of
      set_name -> call_set_name(Call#rpc_call.id, Params, Req, Ctx);
      _ -> io:format("unprocessed method: ~p~n", [Method])
    end
  
%    io:format("~p~n", [{Method, Params}]),
%    io:format("====>~p~n=-=->~p~n", [Call, Body]),
%    {PropList} = Json,
%    Name = proplists:get_value(<<"name">>, PropList),
%    Result = room:name_observer(
%      Ctx#room_context.room_pid,
%      Ctx#room_context.observer_id,
%      Name),
%    io:format("---->~p~n", [Result]),
%    RespJson = {[{<<"status">>, <<"ok">>}]},
%    {true, wrq:set_resp_body(jiffy:encode(RespJson), Req), Ctx}
  catch
    {error, {_, invalid_json}} ->
      {false, Req, Ctx};
    X ->
      io:format("Unexpected error: ~p~n", [X]),
      {false, Req, Ctx}
  end.

call_set_name(
      Id,
      Params = #set_name{who = NamedId, name = Name},
      Req,
      Ctx = #room_context{observer_id = NamedId, room_pid = RoomPid}) ->
  ToJif = case room:name_observer(RoomPid, NamedId, Name) of
    ok ->
      #rpc_response{id = Id, result = Params};
    {error, Code} when is_atom(Code) ->
      #rpc_response{id = Id, error = atom_to_binary(Code, utf8)}
  end,
  Json = jiffy:encode(json_rpc:jif(ToJif)),
  {true, wrq:set_resp_body(Json, Req), Ctx}.

to_json(Req, Ctx) ->
  case wrq:method(Req) of
    'GET' ->
      {ok, When, RoomState} = room:get_state(Ctx#room_context.room_pid),
      Json = json_view:room(RoomState, When, Ctx#room_context.observer_id),
      {jiffy:encode(Json), Req, Ctx};
    'POST' ->
      {jiffy:encode({[]}), Req, Ctx}
  end.


