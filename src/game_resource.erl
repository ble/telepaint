-module(game_resource).
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
  {not room_http:authorized(Ctx), Req, Ctx}.

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

    Id = Call#rpc_call.id,
    Params = Call#rpc_call.params,
    Method = element(1, Params),
    case Method of
      draw ->
        {Success, Response} = case room:game_action(
            Ctx#room_context.room_pid, 
            Ctx#room_context.observer_id,
            Params) of
          ok ->
            {true, #rpc_response{id = Id, result = [<<"ok">>]}};
          {error, Atom} when is_atom(Atom) ->
            Error = #rpc_response_error{message = list_to_binary(atom_to_list(Atom))},
            {false, #rpc_response{id = Id, error = Error}};
          {error, Something} ->
            Error = #rpc_response_error{message = Something},
            {false, #rpc_response{id = Id, error = Error}}
        end,
        JsonResult = jiffy:encode(json_rpc:jif(Response)),
        io:format("All right, we call it a draw.~n", []),
        {Success, wrq:set_resp_body(JsonResult, Req), Ctx};
      _ ->
        call_unprocessed(Id, Method, Req, Ctx)
    end
  catch
    {error, {_, invalid_json}} ->
      {false, Req, Ctx};
    X ->
      io:format("Unexpected error: ~p~n", [X]),
      {false, Req, Ctx}
  end.

call_unprocessed(
  RpcId,
  Method,
  Req,
  Ctx) ->
    io:format("unprocessed method: ~p~n", [Method]),
    Error = #rpc_response_error{message = <<"Unknown method">>},
    Response = #rpc_response{error = Error, id = RpcId},
    JsonResponse = json_rpc:jif(Response),
    {true, wrq:set_resp_body(jiffy:encode(JsonResponse), Req), Ctx}.

to_json(Req, Ctx) ->
  case wrq:method(Req) of
    'GET' ->
      {ok, When, GameState} = room:get_game_state(Ctx#room_context.room_pid),
      Json = json_view:game(GameState, When, Ctx#room_context.observer_id),
      Response = #rpc_response{result = Json},
      {jiffy:encode(json_rpc:jif(Response)), Req, Ctx};
    'POST' ->
      {jiffy:encode({[]}), Req, Ctx}
  end.

