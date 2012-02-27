-module(observer_queue_resource).

-export([
    init/1
   ,service_available/2
   ,allowed_methods/2
   ,resource_exists/2
   ,forbidden/2

   ,content_types_provided/2
   ,to_json/2
 ]).

-include("room_context.hrl").
-include("rpc.hrl").
-include("rpc_methods.hrl").
-include_lib("webmachine/include/webmachine.hrl").

out_of_date(_, _) -> false.

init([]) ->
  {ok, undefined}.

service_available(Req, _) ->
  {true, Req, room_http:req_context(Req)}.

allowed_methods(Req, Ctx) ->
  {['GET'], Req, Ctx}.

%TODO: stub
resource_exists(Req, _) ->
  Ctx = room_http:req_context(Req),
  RoomExists = Ctx#room_context.room_pid =/= undefined,
  {RoomExists, Req, Ctx}.

%TODO: stub
forbidden(Req, Ctx) ->
  {not room_http:authorized(Ctx), Req, Ctx}.

content_types_provided(Req, Ctx) ->
  {[{"application/json", to_json}], Req, Ctx}.

extract_timestamp(Req, Ctx) ->
  Keys = ["mega", "unit", "micro"],
  Parts = [wrq:get_qs_value(Key, Req) || Key <- Keys],
  Missing = lists:any(fun(X) -> X =:= undefined end, Parts),
  Timestamp = case Missing of
    true ->
      {0,0,0};
    false ->
      [Mega, Unit, Micro] = [list_to_integer(X) || X <- Parts],
      {Mega, Unit, Micro}
  end,
  Ctx#room_context{args = Timestamp}.

to_json(Req, Ctx0) ->
  Ctx1 = extract_timestamp(Req, Ctx0),
  Timestamp = Ctx1#room_context.args,
  {LastWhen, Msgs} = player_queue:poll_after(Ctx1#room_context.observer_pid, Timestamp),
  {A, B, C} = LastWhen,
  Response = #rpc_response{result =
    #queue_update{time = [A, B, C], messages = Msgs}},
  {jiffy:encode(json_rpc:jif(Response)), Req, Ctx1}.


