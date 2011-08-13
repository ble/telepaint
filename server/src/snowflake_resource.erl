%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(snowflake_resource).
-export([init/1, content_types_provided/2, allowed_methods/2, forbidden/2]).
-export([encodings_provided/2]).
-export([content_types_accepted/2, from_json/2]).
-export([resource_exists/2, previously_existed/2, moved_permanently/2]).
-export([to_json/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("snowflake.hrl").

init([]) -> {ok, stateless}.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}, {"text/json", to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{"application/json", from_json}], Req, State}.

from_json(Req, State) ->
  io:format("content type recognized~n", []),
  {process_post(Req, State), Req, State}.

encodings_provided(Req, State) ->
  Encs = case wrq:method(Req) of
    'GET' ->
      [{"identity", fun(X) -> X end},
       {"gzip", fun(X) -> zlib:gzip(X) end}];
    _ ->
      [{"identity", fun(X) -> X end}]
  end,
  {Encs, Req, State}.

allowed_methods(Req, stateless) ->
  {['GET', 'HEAD', 'POST'], Req, stateless}.

forbidden(Req, stateless) ->
  Snowflakes = get_snowflake(Req),
  io:format("Requested snowflake: ~p~n", [Snowflakes]),
  case Snowflakes of
    bad_path ->
      {false, Req, {snowflake, undefined}};
    [] ->
      {false, Req, {snowflake, undefined}}; 
    [Snowflake] ->
      SessionId = get_session_id(Req), 
      io:format("snowflake access: ~p~n", [Snowflake#snowflake.session_access]),
      Access = case Snowflake#snowflake.session_access of
        AccessList when is_list(AccessList) ->
          proplists:get_value(SessionId, AccessList);
        AccessType ->
          AccessType
      end,
      io:format("access: ~p~nmethod: ~p~n", [Access, wrq:method(Req)]),
      Forbidden = case {Access, wrq:method(Req)} of
        {write, _} -> false;
        {read, 'POST'} -> true;
        {read, _} -> false;
        _ -> true
      end,
      {Forbidden, Req, {snowflake, Snowflake}}
  end.



resource_exists(Req, {snowflake, undefined}) ->
  {false, Req, {snowflake, undefined}}; 
resource_exists(Req, State = {snowflake, #snowflake{complete_url = undefined}}) ->
  {true, Req, State};
resource_exists(Req, State) ->
  {false, Req, State}.

previously_existed(Req, {snowflake, undefined}) ->
  {false, Req, undefined};
previously_existed(Req, State) ->
  {true, Req, State}.

moved_permanently(Req, State = {snowflake, #snowflake{complete_url = Url}}) ->
  {{true, Url}, Req, State}.


to_json(Req, {snowflake, Snowflake = #snowflake{id = SnowflakeId}}) ->
  FragT = fun() -> mnesia:index_read(snowflake_fragment, SnowflakeId, #snowflake_fragment.id) end,
  io:format("#snowflake_fragment.id -> ~p~n", [#snowflake_fragment.id]),
  T = mnesia:transaction(FragT),
  case T of
    {atomic, JsonFragments} ->
      {assemble_json(Snowflake, JsonFragments), Req, Snowflake};
    {error, Reason} ->
      {io_lib:format("'~p'", [Reason]), Req, Snowflake}
  end.

process_post(Req, {snowflake, #snowflake{id = SnowflakeId}}) ->
  Body0 = wrq:req_body(Req),
  Body1 = jiffy:decode(Body0),
  io:format("json: ~p~n", [Body1]),
  {ok, {Method0, Data, _}} = tpaint_rpc:plain(Body1),
  Method = case Method0 of
    <<"erase-polyline">> -> stroke;
    <<"undo">> -> undo
  end,
  io:format("{method, data}: ~p~n", [{Method, Data}]),
  Stamp = now(),
  InsertT = fun() -> mnesia:write(#snowflake_fragment{timestamp=now(), id=SnowflakeId, json=Body1}) end,
      case mnesia:transaction(InsertT) of
    {atomic, ok} ->
      SuccessBody = jiffy:encode({[{status, ok}, {stamp, tuple_to_list(Stamp)}]}),
      {true, wrq:set_resp_body(SuccessBody, Req), stateless};
    _ ->
      EBody = jiffy:encode({[{status, error}]}),
      {false, wrq:set_resp_body(EBody, Req), stateless}
  end.

  
get_snowflake(Req) ->
  Dp = wrq:disp_path(Req),
  Pir = wrq:path_info(snowflake_id, Req),
  io:format("disp_path -> ~p~npath_info(snowflake_id) -> ~p~n", [Dp, Pir]),
  case wrq:disp_path(Req) of
    [] ->
      case wrq:path_info(snowflake_id, Req) of
        undefined ->
          no_snowflake;
        SnowflakeId0 ->
          SnowflakeId = erlang:list_to_binary(SnowflakeId0),
          io:format("Requested snowflake id: ~p~n", [SnowflakeId]),
          case mnesia:transaction(fun() -> mnesia:read({snowflake, SnowflakeId}) end) of
            {atomic, Result} ->
              Result;
            _ ->
              error
          end
      end;
    _ ->
      bad_path
  end.

get_session_id(Req) ->
  wrq:get_cookie_value("session", Req).

assemble_json(Snowflake, Fragments) ->
  io:format("foo~n", []),
  FragmentJson = [F#snowflake_fragment.json || F <- Fragments],
  Eson = {[{<<"id">>, Snowflake#snowflake.id}, {<<"fragments">>, FragmentJson}]},
  io:format("eson output: ~p~n", [Eson]),
  jiffy:encode(Eson).
