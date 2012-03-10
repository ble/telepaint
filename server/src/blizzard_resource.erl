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
