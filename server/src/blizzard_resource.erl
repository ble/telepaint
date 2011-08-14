%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(blizzard_resource).
-export([init/1, content_types_provided/2, allowed_methods/2]).
-export([resource_exists/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("snowflake.hrl").

init([]) -> {ok, stateless}.

content_types_provided(Req, stateless) ->
  {[{"application/json", to_json}, {"text/json", to_json}], Req, stateless}.

allowed_methods(Req, stateless) ->
  {['GET', 'HEAD'], Req, stateless}.

resource_exists(Req, stateless) ->
  {true, Req, stateless}.

to_json(Req, stateless) ->
  case mnesia:transaction(snowflake_db:all_ids()) of
    {atomic, Ids} ->
      Eson = {[{flakeUrls, Ids}]},
      {jiffy:encode(Eson), Req, stateless};
    {error, Reason} ->
      {io_lib:format("'~p'", [Reason]), Req, stateless}
  end.
% 
%get_snowflake(Req) ->
%  Dp = wrq:disp_path(Req),
%  Pir = wrq:path_info(snowflake_id, Req),
%  io:format("disp_path -> ~p~npath_info(snowflake_id) -> ~p~n", [Dp, Pir]),
%  case wrq:disp_path(Req) of
%    [] ->
%      case wrq:path_info(snowflake_id, Req) of
%        undefined ->
%          no_snowflake;
%        SnowflakeId0 ->
%          SnowflakeId = erlang:list_to_binary(SnowflakeId0),
%          io:format("Requested snowflake id: ~p~n", [SnowflakeId]),
%          case mnesia:transaction(fun() -> mnesia:read({snowflake, SnowflakeId}) end) of
%            {atomic, Result} ->
%              Result;
%            _ ->
%              error
%          end
%      end;
%    _ ->
%      bad_path
%  end.
%
%get_session_id(Req) ->
%  wrq:get_cookie_value("session", Req).
%
%assemble_json(Snowflake, Fragments) ->
%  io:format("foo~n", []),
%  FragmentJson = [F#snowflake_fragment.json || F <- Fragments],
%  Eson = {[{<<"id">>, Snowflake#snowflake.id}, {<<"fragments">>, FragmentJson}]},
%  io:format("eson output: ~p~n", [Eson]),
%  jiffy:encode(Eson).
