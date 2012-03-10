-module(lobby_resource).
-export([init/1, content_types_provided/2, allowed_methods/2]).
-export([resource_exists/2, to_json/2]).
-include_lib("webmachine/include/webmachine.hrl").


init([]) -> {ok, stateless}.

content_types_provided(Req, stateless) ->
  {[{"application/json", to_json}], Req, stateless}.

allowed_methods(Req, stateless) ->
  {['GET', 'HEAD'], Req, stateless}.

resource_exists(Req, stateless) ->
  {true, Req, stateless}.

to_json(Req, stateless) -> 
  Session = wrq:get_cookie_value("session", Req),
  io:format("~p~n", [Session]),
  {<<"\"ok\"">>, Req, stateless}.

