-module(new_room_resource).
-export([init/1]).
-export([
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    to_html/2,
    process_post/2

  ]).

-define(FORM_PATH, "priv/www/new_room.html").
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
  {ok, Value} = file:read_file(?FORM_PATH),
  {Value, Req, Ctx}.


get_room_name(Req) ->
  ContentType = wrq:get_req_header("Content-Type", Req),
  Body = wrq:req_body(Req),
  ParsedBody = mochiweb_util:parse_qs(Body),
  RoomName = proplists:get_value("roomName", ParsedBody),
  io:format("~p~n", [{ContentType, RoomName}]),
  case {ContentType, RoomName} of
    {_, undefined} ->
      error;
    {"application/x-www-form-urlencoded", X}
      when X =/= undefined andalso is_list(X) ->
      {ok, list_to_binary(RoomName)};
    _ ->
      error
  end.

process_post(Req0, Ctx) ->
  case get_room_name(Req0) of
    {ok, Name} ->
%  %create a room
      {ok, Pid, RoomId} = room:start_link(Name),
      ok = nexus:register_room(Pid, RoomId),
%  %create the first user for that room
      {ok, ObserverId} = room:add_observer(Pid),
      Cookie1 = mochiweb_cookies:cookie("roomId", RoomId),
%  %  set cookie identifying user
      Cookie2 = mochiweb_cookies:cookie("observerId", ObserverId),
%  %  303 redirect to the url for the room client
      Loc = {"Location", ["/room/"] ++ binary_to_list(RoomId) ++ "/client"},
      Req1 = wrq:merge_resp_headers([Loc, Cookie1, Cookie2], Req0),
      Req2 = wrq:do_redirect(true, Req1),
      {true, Req2, Ctx};
    _ ->
      {false, Req0, Ctx}
  end.

