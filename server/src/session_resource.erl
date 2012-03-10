-module(session_resource).
-export([init/1, resource_exists/2, content_types_provided/2]).
-export([allowed_methods/2, process_post/2, to_json/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("session.hrl").

init([]) -> {ok, stateless}.

resource_exists(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}], Req, State}.

to_json(Req, State) ->

  Resp = case wrq:get_cookie_value("session", Req) of
    undefined ->
      <<"\"ok\"">>;
    Session0 ->
      Session1 = list_to_binary(Session0),
      case mnesia:transaction(session_db:get(Session1)) of
        {atomic, [Session2]} ->
          [<<"\"">>, Session2#session.name, <<"\"">>];
        X ->
          io:format("~p -> ~p~n", [Session1, X]),
          <<"\"error\"">>
      end
  end, 
  {Resp, Req, State}.

allowed_methods(Req, State) ->
  {['GET', 'HEAD', 'POST'], Req, State}.


get_req_name(Req) ->
  try jiffy:decode(wrq:req_body(Req)) of
    {Items} when is_list(Items) ->
      case proplists:lookup(<<"name">>, Items) of
        {<<"name">>, Name} when is_binary(Name)-> {ok, Name};
        none -> {error, no_name}
      end;
    _X ->
      {error, no_name}
  catch
    _ -> {error, no_parse}
  end.

process_post(Req, State) ->
  case get_req_name(Req) of
    {ok, Name} ->
      %create a new session
      io:format("~p ~p ~n", [Name, list_to_binary(wrq:peer(Req))]),
      {atomic, Session} = mnesia:transaction(session_db:make_session(Name, list_to_binary(wrq:peer(Req)))),
      CookieHeader = "Set-Cookie",
      CookieBody = lists:flatten(io_lib:format("session=~s", [Session#session.id])),
      Req1 = wrq:set_resp_header(CookieHeader, CookieBody, Req),
      Req2 = wrq:set_resp_body(<<"\"ok\"">>, Req1),
      {true, Req2, State};
    {error, _Reason} ->
      {false, Req, State}
  end.
