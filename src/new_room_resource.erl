-module(new_room_resource).
-export([init/1]).
-export([
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_html/2,
    process_post/2

  ]).

-define(FILEPATH, "priv/static/new_room.html").
-include_lib("webmachine/include/webmachine.hrl").

init(_ConfigProps) ->
  {ok, nothing}.

is_authorized(Req, Ctx) ->
  case wrq:peer(Req) of
    "127.0.0.1" ->
      {true, Req, Ctx};
    _ ->
      {false, Req, Ctx}
  end.

allowed_methods(Req, Ctx) ->
  {['GET', 'POST'], Req, Ctx}.

content_types_provided(Req, Ctx) ->
  {[{"text/html", to_html}], Req, Ctx}.

to_html(Req, Ctx) -> 
  {ok, Value} = file:read_file(?FILEPATH),
  {Value, Req, Ctx}.

process_post(Req, Ctx) ->
  {true, Req, Ctx}.
  
