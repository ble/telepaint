-module(observer_queue_resource).
-include("room_context.hrl").

-export([
    init/1
   ,service_available/2
%   ,allowed_methods/2
%   ,resource_exists/2
%   ,forbidden/2
%
%   ,content_types_provided/2
%   ,to_json/2
 ]).


out_of_date(_, _) -> false.

init([]) ->
  {ok, #room_context{}}.

service_available(Req, _) ->
  {true, Req, room_http:req_context(Req)}.

allowed_methods(Req, Ctx) ->
  {['GET'], Req, Ctx}.
