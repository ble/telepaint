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
-include_lib("webmachine/include/webmachine.hrl").

out_of_date(_, _) -> false.

init([]) ->
  {ok, #room_context{}}.

service_available(Req, _) ->
  {true, Req, asfd}.%room_http:req_context(Req)}.

allowed_methods(Req, Ctx) ->
  {['GET'], Req, Ctx}.

%TODO: stub
resource_exists(Req, Ctx) ->
  {true, Req, Ctx}.

%TODO: stub
forbidden(Req, Ctx) ->
  {false, Req, Ctx}.

content_types_provided(Req, Ctx) ->
  {[{"application/json", to_json}], Req, Ctx}.

to_json(Req, Ctx) ->
  {A, B, C} = now(),
  Response = #rpc_call{params = {[{<<"when">>, [A, B, C]}]}},
  {jiffy:encode(json_rpc:jif(Response)), Req, Ctx}.


